use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::iter::zip;
use std::slice::Iter;

/// Continuation bit mask.
const MASK_1000_0000: u8 = 0b1000_0000;

/// Varint "payload" 7 bits mask.
const MASK_0111_1111: u64 = (!MASK_1000_0000) as u64;

// TODO: not clear what's the point of these two functions
// since the same result could be achieved with casting,
// using "as" keyword.
/// Convert u64 representation to i64.
///
/// For example: 0xffff_ffff_ffff_ffff -> -1
pub fn u64_to_i64(v: u64) -> i64 {
    i64::from_ne_bytes(v.to_ne_bytes())
}

/// Convert i64 to u64 representation.
///
/// For example: -1 -> 0xffff_ffff_ffff_ffff
pub fn i64_to_u64(v: i64) -> u64 {
    u64::from_ne_bytes(v.to_ne_bytes())
}

/// Parse varint.
///
/// Return None if the buffer is not valid varint.
pub fn parse_varint(buf: &[u8]) -> Option<(u64, usize)> {
    if is_varint(buf) {
        Some(unsafe_parse_varint(buf))
    } else {
        None
    }
}

fn is_varint(buf: &[u8]) -> bool {
    let mut count = 0;
    for byte in buf {
        count += 1;
        if count == 9 || byte & MASK_1000_0000 == 0 {
            return true;
        }
    }
    false
}

/// Parse varint without validation.
pub fn unsafe_parse_varint(buf: &[u8]) -> (u64, usize) {
    assert!(!buf.is_empty());
    if buf[0] & MASK_1000_0000 == 0 {
        (buf[0] as u64, 1)
    } else {
        let mut v = 0;
        for (i, val) in buf.iter().enumerate().take(8) {
            // 300 --> 0000_0001 0010_1100
            // 000_0010 010_1100
            // [1]000_0010 [0]010_1100
            v <<= 7;
            // remove continuation bit and add 7 bits to v
            v |= *val as u64 & MASK_0111_1111;
            if *val & MASK_1000_0000 == 0 {
                return (v, i + 1);
            }
        }
        v <<= 8;
        v |= buf[8] as u64;
        (v, 9)
    }
}

/// Length of varint in the buffer.
pub fn varint_buf_len(buf: &[u8]) -> Option<usize> {
    for (i, val) in buf.iter().enumerate().take(8) {
        if *val & MASK_1000_0000 == 0 {
            return Some(i + 1)
        }
    }
    if buf.len() >= 9 {
        Some(9)
    } else {
        None
    }
}

#[allow(dead_code)]
pub fn unsafe_varint_buf_len(buf: &[u8]) -> usize {
    for (i, val) in buf.iter().enumerate().take(8) {
        if *val & MASK_1000_0000 == 0 {
            return i + 1
        }
    }
    assert!(buf.len() >= 9);
    9
}

/// Return the length of varint.
pub fn varint_len(v: u64) -> usize {
    let mut i = 1;
    let mut v = v;
    loop {
        v >>= 7;
        if v == 0 {
            break;
        }
        i += 1;
    }
    if i < 10 {
        i
    } else {
        // The original sqlite3VarintLen() may return 10 if the value's MSB is 1.
        // However, practically SQLite does not pass such big value to len_varint().
        // So they don't think it's a bug.
        // https://sqlite-users.sqlite.narkive.com/J3jNoCns/sqlite-assertion-hit-in-sqlite3varintlen
        assert_eq!(i, 10);
        9
    }
}

/// Put varint into the `buf`.
///
/// Return the number of bytes written.
///
/// The `buf` must have at least 9 bytes.
pub fn put_varint(buf: &mut [u8], v: u64) -> usize {
    // 300 --> 0000_0001 0010_1100
    // 000_0010 010_1100
    // [1]000_0010 [0]010_1100
    let mut v = v;
    // check if the value takes 9 bytes
    if v & (0xff << 56) != 0 {
        assert!(buf.len() >= 9);
        buf[8] = v as u8;
        v >>= 8;
        for val in buf[..8].iter_mut().rev() {
            *val = (v & MASK_0111_1111) as u8 | MASK_1000_0000;
            v >>= 7;
        }
        9
    } else {
        let mut reversed_buf = [0u8; 9];
        let mut i = 0;
        loop {
            assert!(i < 9);
            reversed_buf[i] = (v & MASK_0111_1111) as u8 | MASK_1000_0000;
            v >>= 7;
            i += 1;
            if v == 0 {
                break;
            }
        }
        // set the last continuation bit to "0"
        reversed_buf[0] &= MASK_0111_1111 as u8;
        assert!(buf.len() >= i);
        for (v1, v2) in reversed_buf[..i].iter().rev().zip(buf.iter_mut()) {
            *v2 = *v1;
        }
        i
    }
}

/// Whether the byte is a space or not.
///
/// u8::is_ascii_whitespace() is not usable because
/// it does not include b'\x0b' (vertical tab).
#[inline]
pub fn is_space(c: u8) -> bool {
    c == b' ' || (b'\t'..=b'\r').contains(&c)
}

const MAX_I64_PLUS_ONE: u64 = 9223372036854775808;

/// Result of parse_integer().
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParseIntegerResult {
    /// parsed integer
    Integer(i64),
    /// 9223372036854775808
    MaxPlusOne,
    /// exceeds i64 range
    TooBig(bool),
    /// no digits
    Empty,
}

/// Parse integer literal.
///
/// This is inspired by `sqlite3Atoi64()` of SQLite.
///
/// This ignores leading and tailing spaces.
///
/// The first bool value returned indicates whether the input is valid or not.
/// Invalid inputs are:
/// - contains non-digit characters
/// - contains no digits
pub fn parse_integer(input: &[u8]) -> (bool, ParseIntegerResult) {
    let mut pos = 0;
    // skip leading spaces
    for byte in input {
        if !is_space(*byte) {
            break;
        }
        pos += 1;
    }
    // sign
    let is_positive = match input.get(pos) {
        Some(b'-') => {
            pos += 1;
            false
        },
        Some(b'+') => {
            pos += 1;
            true
        },
        _ => true,
    };
    // skip leading zeros
    let mut n_zeros = 0;
    for byte in &input[pos..] {
        if *byte != b'0' {
            break;
        }
        n_zeros += 1;
    }
    // parse digits
    let mut int: u64 = 0;
    let mut n_digits = 0;
    for &byte in &input[pos + n_zeros..] {
        if !byte.is_ascii_digit() {
            break;
        }
        int = int.wrapping_mul(10).wrapping_add((byte - b'0') as u64);
        n_digits += 1;
    }
    // check for extra non-digits at the tail
    let mut is_valid = true;
    for byte in &input[pos + n_zeros + n_digits..] {
        if !is_space(*byte) {
            is_valid = false;
            break;
        }
    }
    if n_zeros + n_digits == 0 {
        (false, ParseIntegerResult::Empty)
    } else if n_digits < 19 {
        let v = if is_positive { int as i64 } else { -(int as i64) };
        (is_valid, ParseIntegerResult::Integer(v))
    } else if n_digits > 19 || int > MAX_I64_PLUS_ONE {
        (is_valid, ParseIntegerResult::TooBig(is_positive))
    } else if int == MAX_I64_PLUS_ONE {
        if is_positive {
            (is_valid, ParseIntegerResult::MaxPlusOne)
        } else {
            (is_valid, ParseIntegerResult::Integer(i64::MIN))
        }
    } else {
        // branch where int is 19 digits and less than MAX_I64_PLUS_ONE
        assert!(int < MAX_I64_PLUS_ONE);
        let v = if is_positive { int as i64 } else { -(int as i64) };
        (is_valid, ParseIntegerResult::Integer(v))
    }
}

/// Parse float literal.
///
/// This is inspired by `sqlite3AtoF()` of SQLite.
///
/// This ignores leading and tailing spaces.
///
/// The input may:
/// - contain '.'
/// - contain 'e' or 'E'
/// - overflow i64 range
pub fn parse_float(input: &[u8]) -> (bool, bool, f64) {
    let mut pos = 0;
    let mut is_integer = false;
    // skip leading spaces
    for byte in input {
        if !is_space(*byte) {
            break;
        }
        pos += 1;
    }
    // sign
    let is_positive = match input.get(pos) {
        Some(b'-') => {
            pos += 1;
            false
        },
        Some(b'+') => {
            pos += 1;
            true
        },
        _ => true,
    };
    // skip leading zeros
    let mut n_zeros = 0;
    for byte in &input[pos..] {
        if *byte != b'0' {
            break;
        }
        n_zeros += 1;
        is_integer = true;
    }
    pos += n_zeros;
    let mut significand = 0;
    let mut n_digits = 0;
    // parse integer part
    for byte in &input[pos..] {
        if byte.is_ascii_digit() {
            significand = significand * 10 + (byte - b'0') as i64;
            n_digits += 1;
            is_integer = true;
            // `(i64::MAX - 9) / 10` creates a threshold value that prevents overflow
            // when multiplying significand by 10 and adding the next digit (0-9)
            if significand < (i64::MAX - 9) / 10 {
                continue;
            }
        }
        break;
    }
    pos += n_digits;
    let mut decimal_point: i64 = 0;
    for byte in &input[pos..] {
        if !byte.is_ascii_digit() {
            break;
        }
        decimal_point += 1;
        pos += 1;
    }
    // parse fractional part
    if let Some(&b'.') = input.get(pos) {
        is_integer = false;
        pos += 1;
        let mut fractional_digits = 0;
        for byte in &input[pos..] {
            if !byte.is_ascii_digit() {
                break;
            }
            if significand < (i64::MAX - 9) / 10 {
                significand = significand * 10 + (byte - b'0') as i64;
                decimal_point -= 1;
            }
            fractional_digits += 1;
        }
        n_digits += fractional_digits;
        pos += fractional_digits;
    }
    // parse exponent part
    let mut is_valid = match input.get(pos) {
        Some(b'e') | Some(b'E') => {
            pos += 1;
            let exponent_sign = match input.get(pos) {
                Some(b'+') => {
                    pos += 1;
                    1
                },
                Some(b'-') => {
                    pos += 1;
                    -1
                },
                _ => -1,
            };
            let mut exponent = 0;
            let mut is_valid_exponent = false;
            for byte in &input[pos..] {
                if !byte.is_ascii_digit() {
                    break;
                }
                exponent = if exponent < 10000 {
                    exponent * 10 + (byte - b'0') as i32
                } else {
                    10000
                };
                pos += 1;
                is_valid_exponent = true;
            }
            decimal_point += (exponent * exponent_sign) as i64;
            if is_valid_exponent {
                is_integer = false;
            }
            is_valid_exponent
        }
        _ => true
    };
    is_valid = is_valid && n_zeros + n_digits > 0;
    // skip leading spaces
    if is_valid {
        for byte in &input[pos..] {
            if !is_space(*byte) {
                is_valid = false;
                break;
            }
        }
    };
    // attempt to reduce exponent
    loop {
        if decimal_point > 0 && significand < i64::MAX / 10 {
            significand *= 10;
            decimal_point -= 1;
        } else if decimal_point < 0 && significand % 10 == 0 {
            significand /= 10;
            decimal_point += 1;
        } else {
            break;
        }
    }
    let v = match decimal_point.try_into() {
        Ok(decimal_point) => significand as f64 * 10.0f64.powi(decimal_point),
        Err(_) => {
            if decimal_point > 0 {
                f64::INFINITY
            } else {
                0.0
            }
        }
    };
    let v = if is_positive { v } else { -v };
    (is_valid, is_integer, v)
}

/// Converts a single byte to its uppercase equivalent.
///
/// This only support ascii characters conversion (i.e. 0 ~ 127).
pub static UPPER_TO_LOWER: [u8; 256] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, // 0x00 - 0x07
    0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, // 0x08 - 0x0F
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, // 0x10 - 0x17
    0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, // 0x18 - 0x1F
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, // 0x20 - 0x27
    0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, // 0x28 - 0x2F
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, // 0x30 - 0x37
    0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, // 0x38 - 0x3F
    // 0x41 ('A') → 0x61 ('a')
    // (0x42) ('B') → 0x62 ('b')
    0x40, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, // 0x40 - 0x47
    0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, // 0x48 - 0x4F
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, // 0x50 - 0x57
    // returns to regular ASCII order at 0x5B ('[')
    0x78, 0x79, 0x7A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, // 0x58 - 0x5F
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, // 0x60 - 0x67
    0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, // 0x68 - 0x6F
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, // 0x70 - 0x77
    0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F, // 0x78 - 0x7F
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, // 0x80 - 0x87
    0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, // 0x88 - 0x8F
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, // 0x90 - 0x97
    0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, // 0x98 - 0x9F
    0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, // 0xA0 - 0xA7
    0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, // 0xA8 - 0xAF
    0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, // 0xB0 - 0xB7
    0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, // 0xB8 - 0xBF
    0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, // 0xC0 - 0xC7
    0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, // 0xC8 - 0xCF
    0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, // 0xD0 - 0xD7
    0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, // 0xD8 - 0xDF
    0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, // 0xE0 - 0xE7
    0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, // 0xE8 - 0xEF
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, // 0xF0 - 0xF7
    0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF, // 0xF8 - 0xFF
];

pub fn upper_to_lower(buf: &mut [u8]) {
    for byte in buf {
        *byte = UPPER_TO_LOWER[*byte as usize]
    }
}

/// A wrapper for bytes to compare each other case insensitively with zero
/// allocation.
#[derive(Eq)]
pub struct CaseInsensitiveBytes<'a>(&'a [u8]);

impl CaseInsensitiveBytes<'_> {
    /// Compare with lowercase bytes.
    pub fn equal_to_lower_bytes(&self, other: &[u8]) -> bool {
        if self.0.len() != other.len() {
            return false;
        };
        for (i, byte) in self.0.iter().enumerate() {
            if UPPER_TO_LOWER[*byte as usize] != other[i] {
                return false;
            }
        }
        true
    }

    /// Return whether `self` contains the `other` (case-insensitive).
    ///
    /// The other must be lowercase.
    ///
    /// Example:
    /// self = b"Hello World", other = b"WORLD" => true
    /// self = b"Hello World", other = b"foo" => false
    pub fn contains_lower_bytes(&self, other: &[u8]) -> bool {
        if other.is_empty() {
            return true;
        } else if self.0.len() < other.len() {
            return false;
        };
        'main_loop: for (i, byte) in self
            .0
            .iter()
            // number of possible starting positions of `other` within `self`
            .take(self.0.len() - other.len() + 1)
            .enumerate()
        {
            if UPPER_TO_LOWER[*byte as usize] == other[0] {
                // skip 1 because we already checked it above
                for (j, other_byte) in other.iter().skip(1).enumerate() {
                    if UPPER_TO_LOWER[self.0[i + 1 + j] as usize] != *other_byte {
                        continue 'main_loop;
                    }
                }
                return true
            }
        }
        false
    }
}

impl<'a> From<&'a [u8]> for CaseInsensitiveBytes<'a> {
    fn from(value: &'a [u8]) -> Self {
        Self(value)
    }
}

impl<'a> From<&'a Vec<u8>> for CaseInsensitiveBytes<'a> {
    fn from(value: &'a Vec<u8>) -> Self {
        Self(&value[..])
    }
}

impl PartialEq for CaseInsensitiveBytes<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for (i, byte) in self.0.iter().enumerate() {
            if UPPER_TO_LOWER[*byte as usize] != UPPER_TO_LOWER[other.0[i] as usize] {
                return false;
            }
        }
        true
    }
}

impl PartialOrd for CaseInsensitiveBytes<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CaseInsensitiveBytes<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        for (a, b) in zip(self.0, other.0) {
            let cmp = UPPER_TO_LOWER[*a as usize].cmp(&UPPER_TO_LOWER[*b as usize]);
            if cmp != Ordering::Equal {
                return cmp;
            }
        }
        self.0.len().cmp(&other.0.len())
    }
}

impl Hash for CaseInsensitiveBytes<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for byte in self.0 {
            state.write_u8(UPPER_TO_LOWER[*byte as usize])
        }
    }
}

/// A wrapper for bytes which may be quoted string.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct MaybeQuotedBytes<'a>(&'a [u8]);

impl MaybeQuotedBytes<'_> {
    /// Copy the dequoted text to newly allocated Vec<u8>.
    ///
    /// The double delimiter in the quoted text will be converted to single.
    ///
    /// If the text is not quoted, the returned Vec<u8> is the same as the
    /// original one.
    pub fn dequote(&self) -> Vec<u8> {
        match self.0.first() {
            Some(b'"') | Some(b'\'') | Some(b'`') => {
                let delim = self.0[0];
                assert!(self.0.len() >= 2);
                assert_eq!(self.0[self.0.len() - 1], delim);
                let mut result = Vec::with_capacity(self.0.len() - 2);
                let mut iter = self.0[1..self.0.len()-1].iter();
                while let Some(&byte) = iter.next() {
                    // Some formats escape characters by doubling them.
                    // Example: 'It''s a test'.
                    if byte == delim {
                        let next = iter.next();
                        assert_eq!(*next.unwrap(), delim)
                    }
                    result.push(byte)
                }
                result
            }
            _ => self.0.to_vec(),
        }
    }

    pub fn dequote_iter(&self) -> DequotedIter<'_> {
        let delim = self.0.first().map(|v| match v {
            b'"' | b'\'' | b'`' => *v,
            _ => 0,
        }).unwrap_or(0);
        let buf = if delim != 0 {
            &self.0[1..self.0.len() - 1]
        } else {
            self.0
        };
        DequotedIter {
            iter: buf.iter(),
            delim,
        }
    }

    pub fn raw(&self) -> &[u8] {
        self.0
    }
}

impl<'a> From<&'a [u8]> for MaybeQuotedBytes<'a> {
    fn from(value: &'a [u8]) -> Self {
        Self(value)
    }
}

pub struct DequotedIter<'a> {
    iter: Iter<'a, u8>,
    delim: u8,
}

impl<'a> Iterator for DequotedIter<'a> {
    type Item = &'a u8;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(byte) = self.iter.next() {
            if *byte == self.delim {
                self.iter.next()
            } else { Some(byte) }
        } else { None }
    }
}

/// Convert 1 byte ascii hexadecimal character to integer.
///
/// The input must be a valid hexadecimal character, i.e. 0-9, a-f, A-F.
///
/// This function is copied from sqlite3HexToInt().
// For digits '0'-'9' (ASCII 48-57)
//     h >> 6 = 48 >> 6 = 0 (since 48 in binary: 00110000)
//     1 & 0 = 0
//     9 * 0 = 0
//     h += 0 → no change
//     Final: h & 0x0f extracts lower 4 bits
//     Example: '7' (ASCII 55 = 00110111)
//     55 & 0x0f = 7 ✓
//
// For letters 'a'-'f' (ASCII 97-102)
//     h >> 6 = 97 >> 6 = 1 (97 in binary: 01100001)
//     1 & 1 = 1
//     9 * 1 = 9
//     h += 9 → converts to correct value
//     Example: 'a' (ASCII 97)
//     97 + 9 = 106
//     106 & 0x0f = 10 ✓
//
// For letters 'A'-'F' (ASCII 65-70)
//     h >> 6 = 65 >> 6 = 1 (65 in binary: 01000001)
//     1 & 1 = 1
//     9 * 1 = 9
//     h += 9 → converts to correct value
//     Example: 'A' (ASCII 65)
//     65 + 9 = 74
//     74 & 0x0f = 10 ✓
pub fn hex_to_int(h: u8) -> u8 {
    assert!(h.is_ascii_hexdigit());
    let mut h = h;
    h += 9 * (1 & (h >> 6));
    h & 0x0f
}

/// A wrapper for bytes which is hexadecimal data.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct HexedBytes<'a>(&'a [u8]);

impl HexedBytes<'_> {
    pub fn decode(&self) -> Vec<u8> {
        assert_eq!(self.0.len() % 2, 0);
        let mut result = Vec::with_capacity(self.0.len() / 2);
        let mut iter = self.0.iter();
        while let Some(&byte) = iter.next() {
            let high = hex_to_int(byte);
            let low = hex_to_int(*iter.next().unwrap());
            result.push(high << 4 | low);
        }
        result
    }
}

impl<'a> From<&'a [u8]> for HexedBytes<'a> {
    fn from(value: &'a [u8]) -> Self {
        assert_eq!(value.len() % 2, 0);
        Self(value)
    }
}
