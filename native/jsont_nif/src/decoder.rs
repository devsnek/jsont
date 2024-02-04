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
    types::{atom, binary::NewBinary, Binary, Encoder},
    Env, Term,
};
use std::{hint::unreachable_unchecked, io::Write};

rustler::atoms! {
    unexpected,
    end_of_input,
    invalid_number,
    invalid_float,
    invalid_big,
}

pub fn decode<'a>(env: Env<'a>, input: Binary<'a>) -> Result<Term<'a>, Error<'a>> {
    let mut d = Decoder {
        env,
        input,
        stack: Vec::with_capacity(16),
        index: 0,
        scratch: vec![],
    };
    d.decode()
}

enum Entry<'a> {
    Array(Term<'a>),
    Object(Vec<Term<'a>>, Vec<Term<'a>>),
}

struct Decoder<'a> {
    env: Env<'a>,
    stack: Vec<Entry<'a>>,
    input: Binary<'a>,
    index: usize,
    scratch: Vec<u8>,
}

impl<'a> Decoder<'a> {
    fn decode(&mut self) -> Result<Term<'a>, Error<'a>> {
        loop {
            self.skip_whitespace();
            let mut value = match self.peek() {
                Some(b'n') => {
                    self.index += 1;
                    self.parse_ident(b"ull")?;
                    atom::nil().encode(self.env)
                }
                Some(b't') => {
                    self.index += 1;
                    self.parse_ident(b"rue")?;
                    atom::true_().encode(self.env)
                }
                Some(b'f') => {
                    self.index += 1;
                    self.parse_ident(b"alse")?;
                    atom::false_().encode(self.env)
                }
                Some(b'-' | b'0'..=b'9') => self.parse_number()?,
                Some(b'"') => self.parse_string()?,
                Some(b'{') => {
                    self.index += 1;
                    self.skip_whitespace();
                    if self.eat(b'}') {
                        Term::map_new(self.env)
                    } else {
                        let key = self.parse_string()?.to_owned();
                        self.skip_whitespace();
                        self.expect(b':')?;
                        self.stack.push(Entry::Object(vec![key], vec![]));
                        continue;
                    }
                }
                Some(b'[') => {
                    self.index += 1;
                    let list = Term::list_new_empty(self.env);
                    self.skip_whitespace();
                    if self.eat(b']') {
                        list
                    } else {
                        self.stack.push(Entry::Array(list));
                        continue;
                    }
                }
                _ => {
                    return Err(self.unexpected());
                }
            };

            loop {
                match self.stack.last_mut() {
                    None => {
                        self.skip_whitespace();
                        if self.peek().is_some() {
                            return Err(self.unexpected());
                        }
                        return Ok(value);
                    }
                    Some(Entry::Object(_, values)) => {
                        values.push(value);
                        self.skip_whitespace();
                        if self.eat(b',') {
                            self.skip_whitespace();
                            let key = self.parse_string()?.to_owned();
                            let Some(Entry::Object(keys, _)) = self.stack.last_mut() else {
                                // SAFETY: the last item on the stack is always an object.
                                unsafe {
                                    unreachable_unchecked();
                                }
                            };
                            keys.push(key);
                            self.skip_whitespace();
                            self.expect(b':')?;
                            break;
                        }
                        self.expect(b'}')?;
                        let Some(Entry::Object(keys, values)) = self.stack.pop() else {
                            // SAFETY: the last item on the stack is always an object.
                            unsafe {
                                unreachable_unchecked();
                            }
                        };
                        value = match Term::map_from_term_arrays(self.env, &keys, &values) {
                            Ok(v) => v,
                            Err(_) => {
                                // slow path: otp rejects duplciate keys, so we need to
                                // filter it ourselves :/
                                let mut pairs = Vec::new();
                                for (k, v) in keys.into_iter().zip(values) {
                                    let k = k.decode_as_binary()?.as_slice().to_owned();
                                    pairs.push((k, v));
                                }
                                pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
                                pairs.dedup_by(|a, b| a.0 == b.0);
                                let pairs = pairs
                                    .into_iter()
                                    .map(|(k, v)| {
                                        let mut bin = NewBinary::new(self.env, k.len());
                                        bin.as_mut_slice().write_all(&k).unwrap();
                                        (Binary::from(bin).to_term(self.env), v)
                                    })
                                    .collect::<Vec<_>>();
                                Term::map_from_pairs(self.env, &pairs)?
                            }
                        };
                        continue;
                    }
                    Some(Entry::Array(list)) => {
                        *list = list.list_prepend(value);
                        self.skip_whitespace();
                        if self.eat(b',') {
                            break;
                        }
                        self.expect(b']')?;
                        let Some(Entry::Array(list)) = self.stack.pop() else {
                            // SAFETY: the last item on the stack is always an array
                            unsafe { unreachable_unchecked() }
                        };
                        value = list.list_reverse()?;
                        continue;
                    }
                }
            }
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        if self.index < self.input.len() {
            let ch = self.input[self.index];
            self.index += 1;
            Some(ch)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.index < self.input.len() {
            Some(self.input[self.index])
        } else {
            None
        }
    }

    fn eat(&mut self, c: u8) -> bool {
        if self.peek() == Some(c) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn expect(&mut self, c: u8) -> Result<(), Error<'a>> {
        if self.eat(c) {
            Ok(())
        } else {
            Err(self.unexpected())
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(b' ' | b'\n' | b'\t' | b'\r') = self.peek() {
            self.index += 1;
        }
    }

    fn parse_ident(&mut self, ident: &[u8]) -> Result<(), Error<'a>> {
        for expected in ident {
            match self.next_char() {
                None => {
                    return Err(self.end_of_input());
                }
                Some(next) => {
                    if next != *expected {
                        return Err(self.unexpected());
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_number(&mut self) -> Result<Term<'a>, Error<'a>> {
        let mut buf = String::with_capacity(16);
        if self.peek() == Some(b'-') {
            self.index += 1;
            buf.push('-');
        }
        let float = self.scan_integer(&mut buf)?;
        if float {
            match buf.parse::<f64>() {
                Ok(f) => {
                    if f.is_infinite() {
                        Err(self.invalid_float("cannot represent inf"))
                    } else {
                        Ok(f.encode(self.env))
                    }
                }
                Err(e) => Err(self.invalid_float(format!("{}", e).as_str())),
            }
        } else {
            match buf.parse::<num_bigint::BigInt>() {
                Ok(big) => Ok(crate::big::BigInt::from(big).encode(self.env)),
                Err(e) => Err(self.invalid_big(e)),
            }
        }
    }

    fn scan_or_eof(&mut self, buf: &mut String) -> Result<u8, Error<'a>> {
        match self.next_char() {
            Some(b) => {
                buf.push(b as char);
                Ok(b)
            }
            None => Err(self.end_of_input()),
        }
    }

    fn scan_integer(&mut self, buf: &mut String) -> Result<bool, Error<'a>> {
        match self.scan_or_eof(buf)? {
            b'0' => {
                // There can be only one leading '0'.
                match self.peek() {
                    Some(b'0'..=b'9') => Err(self.invalid_number()),
                    _ => self.scan_number(buf),
                }
            }
            b'1'..=b'9' => loop {
                match self.peek() {
                    Some(c @ b'0'..=b'9') => {
                        self.index += 1;
                        buf.push(c as char);
                    }
                    _ => {
                        return self.scan_number(buf);
                    }
                }
            },
            _ => Err(self.invalid_number()),
        }
    }

    fn scan_number(&mut self, buf: &mut String) -> Result<bool, Error<'a>> {
        match self.peek() {
            Some(b'.') => {
                self.scan_decimal(buf)?;
                Ok(true)
            }
            Some(e @ (b'e' | b'E')) => {
                self.scan_exponent(e as char, buf)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn scan_decimal(&mut self, buf: &mut String) -> Result<(), Error<'a>> {
        self.index += 1;
        buf.push('.');

        let mut at_least_one_digit = false;
        while let Some(c @ b'0'..=b'9') = self.peek() {
            self.index += 1;
            buf.push(c as char);
            at_least_one_digit = true;
        }

        if !at_least_one_digit {
            match self.peek() {
                Some(_) => return Err(self.invalid_number()),
                None => return Err(self.end_of_input()),
            }
        }

        match self.peek() {
            Some(e @ (b'e' | b'E')) => self.scan_exponent(e as char, buf),
            _ => Ok(()),
        }
    }

    fn scan_exponent(&mut self, e: char, buf: &mut String) -> Result<(), Error<'a>> {
        self.index += 1;
        buf.push(e);

        match self.peek() {
            Some(b'+') => {
                self.index += 1;
                buf.push('+');
            }
            Some(b'-') => {
                self.index += 1;
                buf.push('-');
            }
            _ => {}
        }

        // Make sure a digit follows the exponent place.
        match self.scan_or_eof(buf)? {
            b'0'..=b'9' => {}
            _ => {
                return Err(self.invalid_number());
            }
        }

        while let Some(c @ b'0'..=b'9') = self.peek() {
            self.index += 1;
            buf.push(c as char);
        }

        Ok(())
    }

    fn parse_string(&mut self) -> Result<Term<'a>, Error<'a>> {
        self.expect(b'"')?;

        self.scratch.clear();

        // Index of the first byte not yet copied into the scratch space.
        let mut start = self.index;

        loop {
            while self.index < self.input.len() && !ESCAPE[self.input[self.index] as usize] {
                self.index += 1;
            }
            if self.index == self.input.len() {
                return Err(self.end_of_input());
            }
            match self.input[self.index] {
                b'"' => {
                    if self.scratch.is_empty() {
                        // Fast path: return a slice of the raw JSON without any
                        // copying.
                        let bin = self.input.make_subbinary(start, self.index - start)?;
                        self.index += 1;
                        return Ok(bin.to_term(self.env));
                    } else {
                        self.scratch
                            .extend_from_slice(&self.input[start..self.index]);
                        self.index += 1;

                        let mut bin = NewBinary::new(self.env, self.scratch.len());
                        bin.as_mut_slice().write_all(&self.scratch)?;
                        let term = Binary::from(bin).to_term(self.env);
                        return Ok(term);
                    }
                }
                b'\\' => {
                    self.scratch
                        .extend_from_slice(&self.input[start..self.index]);
                    self.index += 1;
                    self.parse_escape()?;
                    start = self.index;
                }
                _ => {
                    return Err(self.unexpected());
                }
            }
        }
    }

    fn peek_or_eof(&self) -> Result<u8, Error<'a>> {
        match self.peek() {
            Some(c) => Ok(c),
            None => Err(self.end_of_input()),
        }
    }

    fn parse_escape(&mut self) -> Result<(), Error<'a>> {
        let ch = match self.next_char() {
            Some(ch) => ch,
            None => {
                return Err(self.end_of_input());
            }
        };

        match ch {
            b'"' => self.scratch.push(b'"'),
            b'\\' => self.scratch.push(b'\\'),
            b'/' => self.scratch.push(b'/'),
            b'b' => self.scratch.push(b'\x08'),
            b'f' => self.scratch.push(b'\x0c'),
            b'n' => self.scratch.push(b'\n'),
            b'r' => self.scratch.push(b'\r'),
            b't' => self.scratch.push(b'\t'),
            b'u' => {
                fn encode_surrogate(scratch: &mut Vec<u8>, n: u16) {
                    scratch.extend_from_slice(&[
                        (n >> 12 & 0b0000_1111) as u8 | 0b1110_0000,
                        (n >> 6 & 0b0011_1111) as u8 | 0b1000_0000,
                        (n & 0b0011_1111) as u8 | 0b1000_0000,
                    ]);
                }

                let c = match self.decode_hex_escape()? {
                    n @ 0xDC00..=0xDFFF => {
                        encode_surrogate(&mut self.scratch, n);
                        return Ok(());
                    }

                    // Non-BMP characters are encoded as a sequence of two hex
                    // escapes, representing UTF-16 surrogates. If deserializing a
                    // utf-8 string the surrogates are required to be paired,
                    // whereas deserializing a byte string accepts lone surrogates.
                    n1 @ 0xD800..=0xDBFF => {
                        if self.peek_or_eof()? == b'\\' {
                            self.index += 1;
                        } else {
                            encode_surrogate(&mut self.scratch, n1);
                            return Ok(());
                        }

                        if self.peek_or_eof()? == b'u' {
                            self.index += 1;
                        } else {
                            encode_surrogate(&mut self.scratch, n1);
                            // The \ prior to this byte started an escape sequence,
                            // so we need to parse that now. This recursive call
                            // does not blow the stack on malicious input because
                            // the escape is not \u, so it will be handled by one
                            // of the easy nonrecursive cases.
                            return self.parse_escape();
                        }

                        let n2 = self.decode_hex_escape()?;

                        #[allow(clippy::manual_range_contains)]
                        if n2 < 0xDC00 || n2 > 0xDFFF {
                            return Err(self.custom("lone leadning surrogate in hex escape"));
                        }

                        let n = (((n1 - 0xD800) as u32) << 10 | (n2 - 0xDC00) as u32) + 0x1_0000;

                        match char::from_u32(n) {
                            Some(c) => c,
                            None => return Err(self.custom("invalid unicode codepoint")),
                        }
                    }

                    // Every u16 outside of the surrogate ranges above is guaranteed
                    // to be a legal char.
                    n => char::from_u32(n as u32).unwrap(),
                };

                self.scratch
                    .extend_from_slice(c.encode_utf8(&mut [0_u8; 4]).as_bytes());
            }
            _ => {
                return Err(self.custom("invalid escape"));
            }
        }

        Ok(())
    }

    fn decode_hex_escape(&mut self) -> Result<u16, Error<'a>> {
        if self.index + 4 > self.input.len() {
            self.index = self.input.len();
            return Err(self.end_of_input());
        }

        let mut n = 0;
        for _ in 0..4 {
            let ch = decode_hex_val(self.input[self.index]);
            self.index += 1;
            match ch {
                None => return Err(self.custom("invalid escape")),
                Some(val) => {
                    n = (n << 4) + val;
                }
            }
        }
        Ok(n)
    }

    #[cold]
    fn unexpected(&self) -> Error<'a> {
        Error::tuple(unexpected(), vec![self.index.encode(self.env)])
    }

    #[cold]
    fn invalid_float(&self, msg: &str) -> Error<'a> {
        Error::tuple(
            invalid_float(),
            vec![self.index.encode(self.env), msg.encode(self.env)],
        )
    }

    #[cold]
    fn invalid_big(&self, e: num_bigint::ParseBigIntError) -> Error<'a> {
        Error::tuple(
            invalid_big(),
            vec![
                self.index.encode(self.env),
                format!("{}", e).encode(self.env),
            ],
        )
    }

    #[cold]
    fn custom(&self, message: &str) -> Error<'a> {
        Error::tuple(
            unexpected(),
            vec![self.index.encode(self.env), message.encode(self.env)],
        )
    }

    #[cold]
    fn end_of_input(&self) -> Error<'a> {
        Error::tuple(end_of_input(), vec![self.index.encode(self.env)])
    }

    #[cold]
    fn invalid_number(&self) -> Error<'a> {
        Error::tuple(invalid_number(), vec![self.index.encode(self.env)])
    }
}

static HEX: [u8; 256] = {
    const __: u8 = 255; // not a hex digit
    #[allow(clippy::zero_prefixed_literal)]
    [
        //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 0
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 1
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
        00, 01, 02, 03, 04, 05, 06, 07, 08, 09, __, __, __, __, __, __, // 3
        __, 10, 11, 12, 13, 14, 15, __, __, __, __, __, __, __, __, __, // 4
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 5
        __, 10, 11, 12, 13, 14, 15, __, __, __, __, __, __, __, __, __, // 6
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
    ]
};

fn decode_hex_val(val: u8) -> Option<u16> {
    let n = HEX[val as usize] as u16;
    if n == 255 {
        None
    } else {
        Some(n)
    }
}

// Lookup table of bytes that must be escaped. A value of true at index i means
// that byte i requires an escape sequence in the input.
static ESCAPE: [bool; 256] = {
    const CT: bool = true; // control character \x00..=\x1F
    const QU: bool = true; // quote \x22
    const BS: bool = true; // backslash \x5C
    const __: bool = false; // allow unescaped
    [
        //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, // 0
        CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, // 1
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
    ]
};
