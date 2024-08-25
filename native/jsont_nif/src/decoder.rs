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
    BigInt, Env, Term,
};
use std::{
    collections::{hash_map::RandomState, HashMap},
    hint::unreachable_unchecked,
    io::Write,
};

rustler::atoms! {
    unexpected,
    end_of_input,
    invalid_number,
    invalid_float,
    invalid_big,
}

pub fn decode<'a>(
    env: Env<'a>,
    input: Binary<'a>,
    validate_unicode: bool,
) -> Result<Term<'a>, Error<'a>> {
    let mut d = Decoder {
        env,
        input,
        stack: Vec::with_capacity(16),
        index: 0,
        scratch: vec![],
        validate_unicode,
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
    validate_unicode: bool,
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
                                let map = HashMap::<_, _, RandomState>::from_iter(
                                    keys.into_iter().zip(values),
                                );
                                let pairs = map.into_iter().collect::<Vec<_>>();
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
        let start = self.index;
        if self.peek() == Some(b'-') {
            self.index += 1;
        }
        let float = self.scan_integer()?;
        // SAFETY: scanning has validated this range of characters.
        let slice = unsafe { std::str::from_utf8_unchecked(&self.input[start..self.index]) };
        if float {
            match slice.parse::<f64>() {
                Ok(f) => {
                    if f.is_infinite() {
                        Err(self.invalid_number("cannot represent inf"))
                    } else {
                        Ok(f.encode(self.env))
                    }
                }
                Err(e) => Err(self.invalid_number(e)),
            }
        } else {
            match slice.parse::<i64>() {
                Ok(i) => Ok(i.encode(self.env)),
                Err(_) => match slice.parse::<BigInt>() {
                    Ok(big) => Ok(big.encode(self.env)),
                    Err(e) => Err(self.invalid_number(e)),
                },
            }
        }
    }

    fn scan_or_eof(&mut self) -> Result<u8, Error<'a>> {
        match self.next_char() {
            Some(b) => Ok(b),
            None => Err(self.end_of_input()),
        }
    }

    fn scan_integer(&mut self) -> Result<bool, Error<'a>> {
        match self.scan_or_eof()? {
            b'0' => {
                // There can be only one leading '0'.
                match self.peek() {
                    Some(b'0'..=b'9') => Err(self.invalid_number("unexpected")),
                    _ => self.scan_number(),
                }
            }
            b'1'..=b'9' => loop {
                match self.peek() {
                    Some(b'0'..=b'9') => {
                        self.index += 1;
                    }
                    _ => {
                        return self.scan_number();
                    }
                }
            },
            _ => Err(self.invalid_number("unexpected")),
        }
    }

    fn scan_number(&mut self) -> Result<bool, Error<'a>> {
        match self.peek() {
            Some(b'.') => {
                self.scan_decimal()?;
                Ok(true)
            }
            Some(b'e' | b'E') => {
                self.scan_exponent()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn scan_decimal(&mut self) -> Result<(), Error<'a>> {
        self.index += 1;

        let mut at_least_one_digit = false;
        while let Some(b'0'..=b'9') = self.peek() {
            self.index += 1;
            at_least_one_digit = true;
        }

        if !at_least_one_digit {
            match self.peek() {
                Some(_) => return Err(self.invalid_number("unexpected")),
                None => return Err(self.end_of_input()),
            }
        }

        match self.peek() {
            Some(b'e' | b'E') => self.scan_exponent(),
            _ => Ok(()),
        }
    }

    fn scan_exponent(&mut self) -> Result<(), Error<'a>> {
        self.index += 1;

        match self.peek() {
            Some(b'+') => {
                self.index += 1;
            }
            Some(b'-') => {
                self.index += 1;
            }
            _ => {}
        }

        // Make sure a digit follows the exponent place.
        match self.scan_or_eof()? {
            b'0'..=b'9' => {}
            _ => {
                return Err(self.invalid_number("unexpected"));
            }
        }

        while let Some(b'0'..=b'9') = self.peek() {
            self.index += 1;
        }

        Ok(())
    }

    fn parse_string(&mut self) -> Result<Term<'a>, Error<'a>> {
        self.expect(b'"')?;

        self.scratch.clear();

        // Index of the first byte not yet copied into the scratch space.
        let mut start = self.index;

        loop {
            self.skip_to_escape(true);
            if self.index == self.input.len() {
                return Err(self.end_of_input());
            }
            match self.input[self.index] {
                b'"' => {
                    if self.scratch.is_empty() {
                        // Fast path: return a slice of the raw JSON without any
                        // copying.
                        if self.validate_unicode
                            && simdutf8::basic::from_utf8(&self.input[start..self.index]).is_err()
                        {
                            return Err(self.custom("invalid unicode"));
                        }
                        let bin = self.input.make_subbinary(start, self.index - start)?;
                        self.index += 1;
                        return Ok(bin.to_term(self.env));
                    } else {
                        self.scratch
                            .extend_from_slice(&self.input[start..self.index]);
                        self.index += 1;

                        if self.validate_unicode
                            && simdutf8::basic::from_utf8(&self.scratch).is_err()
                        {
                            return Err(self.custom("invalid unicode"));
                        }

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

    fn skip_to_escape(&mut self, forbid_control_characters: bool) {
        // Immediately bail-out on empty strings and consecutive escapes (e.g. \u041b\u0435)
        if self.index == self.input.len()
            || is_escape(self.input[self.index], forbid_control_characters)
        {
            return;
        }
        self.index += 1;

        let rest = &self.input[self.index..];

        if !forbid_control_characters {
            self.index += memchr::memchr2(b'"', b'\\', rest).unwrap_or(rest.len());
            return;
        }

        // We wish to find the first byte in range 0x00..=0x1F or " or \. Ideally, we'd use
        // something akin to memchr3, but the memchr crate does not support this at the moment.
        // Therefore, we use a variation on Mycroft's algorithm [1] to provide performance better
        // than a naive loop. It runs faster than equivalent two-pass memchr2+SWAR code on
        // benchmarks and it's cross-platform, so probably the right fit.
        // [1]: https://groups.google.com/forum/#!original/comp.lang.c/2HtQXvg7iKc/xOJeipH6KLMJ

        const STEP: usize = std::mem::size_of::<u64>();
        const ONE_BYTES: u64 = u64::MAX / 255; // 0x0101...01

        for chunk in rest.chunks_exact(STEP) {
            let chars = u64::from_le_bytes(chunk.try_into().unwrap());
            let contains_ctrl = chars.wrapping_sub(ONE_BYTES * 0x20) & !chars;
            let chars_quote = chars ^ (ONE_BYTES * u64::from(b'"'));
            let contains_quote = chars_quote.wrapping_sub(ONE_BYTES) & !chars_quote;
            let chars_backslash = chars ^ (ONE_BYTES * u64::from(b'\\'));
            let contains_backslash = chars_backslash.wrapping_sub(ONE_BYTES) & !chars_backslash;
            let masked = (contains_ctrl | contains_quote | contains_backslash) & (ONE_BYTES << 7);
            if masked != 0 {
                // SAFETY: chunk is in-bounds for slice
                self.index = unsafe { chunk.as_ptr().offset_from(self.input.as_ptr()) } as usize
                    + masked.trailing_zeros() as usize / 8;
                return;
            }
        }

        self.index += rest.len() / STEP * STEP;
        self.skip_to_escape_slow();
    }

    #[cold]
    #[inline(never)]
    fn skip_to_escape_slow(&mut self) {
        while self.index < self.input.len() && !is_escape(self.input[self.index], true) {
            self.index += 1;
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
            b'u' => self.parse_unicode_escape()?,
            _ => {
                return Err(self.custom("invalid escape"));
            }
        }

        Ok(())
    }

    fn parse_unicode_escape(&mut self) -> Result<(), Error<'a>> {
        let mut n = self.decode_hex_escape()?;

        // Non-BMP characters are encoded as a sequence of two hex escapes,
        // representing UTF-16 surrogates. If deserializing a utf-8 string the
        // surrogates are required to be paired, whereas deserializing a byte string
        // accepts lone surrogates.
        if self.validate_unicode && (0xDC00..=0xDFFF).contains(&n) {
            return Err(self.unexpected());
        }

        loop {
            if !(0xD800..=0xDBFF).contains(&n) {
                // Every u16 outside of the surrogate ranges is guaranteed to be a
                // legal char.
                push_wtf8_codepoint(n as u32, &mut self.scratch);
                return Ok(());
            }

            // n is a leading surrogate, we now expect a trailing surrogate.
            let n1 = n;

            if self.peek_or_eof()? == b'\\' {
                self.index += 1;
            } else if self.validate_unicode {
                self.index += 1;
                return Err(self.unexpected());
            } else {
                push_wtf8_codepoint(n1 as u32, &mut self.scratch);
                return Ok(());
            }

            if self.peek_or_eof()? == b'u' {
                self.index += 1;
            } else if self.validate_unicode {
                self.index += 1;
                return Err(self.unexpected());
            } else {
                push_wtf8_codepoint(n1 as u32, &mut self.scratch);
                // The \ prior to this byte started an escape sequence, so we
                // need to parse that now. This recursive call does not blow the
                // stack on malicious input because the escape is not \u, so it
                // will be handled by one of the easy nonrecursive cases.
                return self.parse_escape();
            }

            let n2 = self.decode_hex_escape()?;

            if !(0xDC00..=0xDFFF).contains(&n2) {
                if self.validate_unicode {
                    return Err(self.unexpected());
                }
                push_wtf8_codepoint(n1 as u32, &mut self.scratch);
                // If n2 is a leading surrogate, we need to restart.
                n = n2;
                continue;
            }

            // This value is in range U+10000..=U+10FFFF, which is always a valid
            // codepoint.
            let n = (((n1 - 0xD800) as u32) << 10 | (n2 - 0xDC00) as u32) + 0x1_0000;
            push_wtf8_codepoint(n, &mut self.scratch);
            return Ok(());
        }
    }

    fn decode_hex_escape(&mut self) -> Result<u16, Error<'a>> {
        match self.input[self.index..] {
            [a, b, c, d, ..] => {
                self.index += 4;
                match decode_four_hex_digits(a, b, c, d) {
                    Some(val) => Ok(val),
                    None => Err(self.custom("invalid hex escape")),
                }
            }
            _ => {
                self.index = self.input.len();
                Err(self.end_of_input())
            }
        }
    }

    #[cold]
    fn unexpected(&self) -> Error<'a> {
        Error::tuple(unexpected(), vec![self.index.encode(self.env)])
    }

    #[cold]
    fn invalid_number(&self, msg: impl std::fmt::Display) -> Error<'a> {
        Error::tuple(
            invalid_number(),
            vec![
                self.index.encode(self.env),
                format!("{msg}").encode(self.env),
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
}

fn is_escape(ch: u8, including_control_characters: bool) -> bool {
    ch == b'"' || ch == b'\\' || (including_control_characters && ch < 0x20)
}

#[inline]
fn push_wtf8_codepoint(n: u32, scratch: &mut Vec<u8>) {
    if n < 0x80 {
        scratch.push(n as u8);
        return;
    }

    scratch.reserve(4);

    unsafe {
        let ptr = scratch.as_mut_ptr().add(scratch.len());

        let encoded_len = match n {
            0..=0x7F => unreachable!(),
            0x80..=0x7FF => {
                ptr.write((n >> 6 & 0b0001_1111) as u8 | 0b1100_0000);
                2
            }
            0x800..=0xFFFF => {
                ptr.write((n >> 12 & 0b0000_1111) as u8 | 0b1110_0000);
                ptr.add(1).write((n >> 6 & 0b0011_1111) as u8 | 0b1000_0000);
                3
            }
            0x1_0000..=0x10_FFFF => {
                ptr.write((n >> 18 & 0b0000_0111) as u8 | 0b1111_0000);
                ptr.add(1)
                    .write((n >> 12 & 0b0011_1111) as u8 | 0b1000_0000);
                ptr.add(2).write((n >> 6 & 0b0011_1111) as u8 | 0b1000_0000);
                4
            }
            0x11_0000.. => unreachable!(),
        };
        ptr.add(encoded_len - 1)
            .write((n & 0b0011_1111) as u8 | 0b1000_0000);

        scratch.set_len(scratch.len() + encoded_len);
    }
}

const fn decode_hex_val_slow(val: u8) -> Option<u8> {
    match val {
        b'0'..=b'9' => Some(val - b'0'),
        b'A'..=b'F' => Some(val - b'A' + 10),
        b'a'..=b'f' => Some(val - b'a' + 10),
        _ => None,
    }
}

const fn build_hex_table(shift: usize) -> [i16; 256] {
    let mut table = [0; 256];
    let mut ch = 0;
    while ch < 256 {
        table[ch] = match decode_hex_val_slow(ch as u8) {
            Some(val) => (val as i16) << shift,
            None => -1,
        };
        ch += 1;
    }
    table
}

static HEX0: [i16; 256] = build_hex_table(0);
static HEX1: [i16; 256] = build_hex_table(4);

fn decode_four_hex_digits(a: u8, b: u8, c: u8, d: u8) -> Option<u16> {
    let a = HEX1[a as usize] as i32;
    let b = HEX0[b as usize] as i32;
    let c = HEX1[c as usize] as i32;
    let d = HEX0[d as usize] as i32;

    let codepoint = ((a | b) << 8) | c | d;

    // A single sign bit check.
    if codepoint >= 0 {
        Some(codepoint as u16)
    } else {
        None
    }
}
