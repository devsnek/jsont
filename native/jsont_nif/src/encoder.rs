// serde_json license
//
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

use crate::Error;
use rustler::{
    types::{atom, MapIterator},
    Binary, Branch, Encoder as RustlerEncoder, Env, NewBinary, Term, TermType, Yield,
};
use serde_json::ser::{CharEscape, Formatter};
use std::io::Write;

rustler::atoms! {
    cannot_encode,
    internal,
    invalid_object_key,
    __struct__,
}

pub fn encode<'a>(
    env: Env<'a>,
    out: Vec<u8>,
    stack: Term<'a>,
    bigint_as_string: bool,
    strip_elixir_struct: bool,
) -> Branch<'a, Term<'a>> {
    let mut encoder = Encoder {
        stack,
        env,
        bigint_as_string,
        strip_elixir_struct,
        formatter: serde_json::ser::CompactFormatter,
        writer: out,
    };
    match encoder.encode() {
        Ok(branch) => match branch {
            Branch::Stop(()) => {
                let mut bin = NewBinary::new(env, encoder.writer.len());
                if let Err(e) = bin.as_mut_slice().write_all(&encoder.writer) {
                    return Branch::Stop(crate::encode_error(env, e.into()));
                }
                let term = Binary::from(bin).to_term(env);
                Branch::Stop((atom::ok(), term).encode(env))
            }
            Branch::Yield(y) => Branch::Yield(y),
        },
        Err(e) => Branch::Stop(crate::encode_error(env, e)),
    }
}

#[rustler::nif]
fn resume<'a>(
    env: Env<'a>,
    out: Vec<u8>,
    stack: Term<'a>,
    bigint_as_string: bool,
    strip_elixir_struct: bool,
) -> Branch<'a, Term<'a>> {
    encode(env, out, stack, bigint_as_string, strip_elixir_struct)
}

struct Encoder<'a, F> {
    env: Env<'a>,
    stack: Term<'a>,
    bigint_as_string: bool,
    strip_elixir_struct: bool,
    formatter: F,
    writer: Vec<u8>,
}

impl<'a, F> Encoder<'a, F>
where
    F: Formatter,
{
    fn push(&mut self, term: impl RustlerEncoder) {
        self.stack = self.stack.list_prepend(term.encode(self.env));
    }

    fn pop(&mut self) -> Result<Term<'a>, Error<'a>> {
        let (head, tail) = self.stack.list_get_cell()?;
        self.stack = tail;
        Ok(head)
    }

    fn encode(&mut self) -> Result<Branch<'a, ()>, Error<'a>> {
        use TermType::*;

        let mut reds = 0;

        loop {
            if self.stack.is_empty_list() {
                break;
            }

            if reds > 4000 {
                let args = vec![
                    self.writer.encode(self.env),
                    self.stack,
                    self.bigint_as_string.encode(self.env),
                    self.strip_elixir_struct.encode(self.env),
                ];
                return Ok(Branch::Yield(Yield::to(resume, args)));
            }

            reds += 1;

            let term = self.pop()?;
            match term.get_type() {
                Atom => {
                    let string = term.atom_to_string()?;
                    match string.as_str() {
                        "true" => self.formatter.write_bool(&mut self.writer, true)?,
                        "false" => self.formatter.write_bool(&mut self.writer, false)?,
                        "nil" => self.formatter.write_null(&mut self.writer)?,
                        _ => self.write_string(string.as_bytes())?,
                    }
                }
                Binary => {
                    let binary = term.into_binary()?;
                    self.write_string(binary.as_slice())?;
                }
                Integer => {
                    self.write_integer(term, false)?;
                }
                Float => {
                    let f = term.decode::<f64>()?;
                    self.formatter.write_f64(&mut self.writer, f)?;
                }
                List => {
                    self.formatter.begin_array(&mut self.writer)?;
                    match term.list_get_cell() {
                        Ok((head, tail)) => {
                            self.push(tail);
                            self.push(head);
                            self.formatter.begin_array_value(&mut self.writer, true)?;
                            continue;
                        }
                        Err(e) => {
                            if term.is_empty_list() {
                                self.formatter.end_array(&mut self.writer)?;
                            } else {
                                return Err(e.into());
                            }
                        }
                    }
                }
                Map => {
                    self.formatter.begin_object(&mut self.writer)?;
                    let Some(mut iter) = MapIterator::new(term) else {
                        return Err(Error::tuple(
                            internal(),
                            vec!["failed to construct MapIterator".encode(self.env)],
                        ));
                    };
                    match iter.next() {
                        Some((k, v)) => {
                            self.push(term.map_remove(k)?);
                            self.push(v);
                            self.write_object_key(k, true)?;
                            self.formatter.begin_object_value(&mut self.writer)?;
                            continue;
                        }
                        None => {
                            self.formatter.end_object(&mut self.writer)?;
                        }
                    }
                }
                Tuple | Fun | Pid | Port | Ref | Unknown => {
                    return Err(Error::tuple(cannot_encode(), vec![term]));
                }
            }

            loop {
                reds += 1;

                if self.stack.is_empty_list() {
                    break;
                }

                let term = self.pop()?;
                match term.get_type() {
                    List => {
                        self.formatter.end_array_value(&mut self.writer)?;

                        match term.list_get_cell() {
                            Ok((head, tail)) => {
                                self.push(tail);
                                self.push(head);
                                self.formatter.begin_array_value(&mut self.writer, false)?;
                                break;
                            }
                            Err(e) => {
                                if term.is_empty_list() {
                                    self.formatter.end_array(&mut self.writer)?;
                                } else {
                                    return Err(e.into());
                                }
                            }
                        }
                    }
                    Map => {
                        self.formatter.end_object_value(&mut self.writer)?;

                        let Some(mut iter) = MapIterator::new(term) else {
                            return Err(Error::tuple(
                                internal(),
                                vec!["failed to construct MapIterator".encode(self.env)],
                            ));
                        };
                        match iter.next() {
                            Some((k, v)) => {
                                self.push(term.map_remove(k)?);
                                self.push(v);
                                self.write_object_key(k, false)?;
                                self.formatter.begin_object_value(&mut self.writer)?;
                                break;
                            }
                            None => {
                                self.formatter.end_object(&mut self.writer)?;
                            }
                        }
                    }
                    _ => {
                        unreachable!()
                    }
                }
            }
        }

        Ok(Branch::Stop(()))
    }

    fn write_string(&mut self, mut input: &[u8]) -> Result<(), Error<'a>> {
        self.formatter.begin_string(&mut self.writer)?;
        while !input.is_empty() {
            match simdutf8::compat::from_utf8(input) {
                Ok(s) => {
                    format_escaped_str_contents(&mut self.writer, &mut self.formatter, s)?;
                    break;
                }
                Err(error) => {
                    let (valid, after_valid) = input.split_at(error.valid_up_to());

                    // SAFETY: we know that this is the valid section
                    let s = unsafe { std::str::from_utf8_unchecked(valid) };
                    format_escaped_str_contents(&mut self.writer, &mut self.formatter, s)?;

                    let (bytes, after) = if let Some(invalid_sequence_length) = error.error_len() {
                        after_valid.split_at(invalid_sequence_length)
                    } else {
                        (after_valid, &[] as &[u8])
                    };
                    for byte in bytes {
                        self.formatter
                            .write_char_escape(&mut self.writer, CharEscape::AsciiControl(*byte))?;
                    }

                    input = after;
                }
            };
        }
        self.formatter.end_string(&mut self.writer)?;
        Ok(())
    }

    fn write_integer(&mut self, term: Term<'a>, force_string: bool) -> Result<(), Error<'a>> {
        match term.decode::<i64>() {
            Ok(i) => {
                #[allow(clippy::manual_range_contains)]
                let as_string = force_string
                    || (self.bigint_as_string && (i < -9007199254740992 || i > 9007199254740992));
                if as_string {
                    self.formatter.begin_string(&mut self.writer)?;
                }
                self.formatter.write_i64(&mut self.writer, i)?;
                if as_string {
                    self.formatter.end_string(&mut self.writer)?;
                }
            }
            Err(_) => {
                let as_string = force_string || self.bigint_as_string;

                let big = term.decode::<crate::big::BigInt>()?;
                let string = big.to_string();

                if as_string {
                    self.formatter.begin_string(&mut self.writer)?;
                }
                self.formatter.write_number_str(&mut self.writer, &string)?;
                if as_string {
                    self.formatter.end_string(&mut self.writer)?;
                }
            }
        }
        Ok(())
    }

    fn write_object_key(&mut self, term: Term<'a>, first: bool) -> Result<(), Error<'a>> {
        self.formatter.begin_object_key(&mut self.writer, first)?;
        match term.get_type() {
            TermType::Atom => {
                let string = term.atom_to_string()?;
                self.write_string(string.as_bytes())?;
            }
            TermType::Binary => {
                let binary = term.into_binary()?;
                self.write_string(binary.as_slice())?;
            }
            TermType::Float => {
                let f = term.decode::<f64>()?;
                self.formatter.begin_string(&mut self.writer)?;
                self.formatter.write_f64(&mut self.writer, f)?;
                self.formatter.end_string(&mut self.writer)?;
            }
            TermType::Integer => {
                self.write_integer(term, true)?;
            }
            _ => {
                return Err(Error::tuple(invalid_object_key(), vec![term]));
            }
        }
        self.formatter.end_object_key(&mut self.writer)?;
        Ok(())
    }
}

fn format_escaped_str_contents<W, F>(
    writer: &mut W,
    formatter: &mut F,
    value: &str,
) -> std::io::Result<()>
where
    W: ?Sized + Write,
    F: ?Sized + Formatter,
{
    let bytes = value.as_bytes();

    let mut start = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        let escape = ESCAPE[byte as usize];
        if escape == 0 {
            continue;
        }

        if start < i {
            formatter.write_string_fragment(writer, &value[start..i])?;
        }

        let char_escape = match escape {
            self::BB => CharEscape::Backspace,
            self::TT => CharEscape::Tab,
            self::NN => CharEscape::LineFeed,
            self::FF => CharEscape::FormFeed,
            self::RR => CharEscape::CarriageReturn,
            self::QU => CharEscape::Quote,
            self::BS => CharEscape::ReverseSolidus,
            self::UU => CharEscape::AsciiControl(byte),
            _ => unreachable!(),
        };
        formatter.write_char_escape(writer, char_escape)?;

        start = i + 1;
    }

    if start == bytes.len() {
        return Ok(());
    }

    formatter.write_string_fragment(writer, &value[start..])
}

const BB: u8 = b'b'; // \x08
const TT: u8 = b't'; // \x09
const NN: u8 = b'n'; // \x0A
const FF: u8 = b'f'; // \x0C
const RR: u8 = b'r'; // \x0D
const QU: u8 = b'"'; // \x22
const BS: u8 = b'\\'; // \x5C
const UU: u8 = b'u'; // \x00...\x1F except the ones above
const __: u8 = 0;

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in JSON. A value of 0 means that byte i is not escaped.
static ESCAPE: [u8; 256] = [
    //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
    UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
    __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
    __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
];
