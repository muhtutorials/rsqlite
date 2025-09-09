const MASK_1000_0000: u8 = 0b1000_0000;
const MASK_0111_1111: u64 = (!MASK_1000_0000) as u64;

// TODO: not clear what's the point of these two functions
// since the same result could be achieved with casting
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

/// Parse varint without validation
pub fn unsafe_parse_varint(buf: &[u8]) -> (u64, usize) {
    assert!(!buf.is_empty());
    if buf[0] & MASK_1000_0000 == 0 {
        (buf[0] as u64, 1)
    } else {
        let mut ret = 0;
        for (i, val) in buf.iter().enumerate().take(8) {
            // 300 --> 0000_0001 0010_1100
            // 000_0010 010_1100
            // [1]000_0010 [0]010_1100
            ret <<= 7;
            ret |= *val as u64 & MASK_0111_1111;
            if *val & MASK_1000_0000 == 0 {
                return (ret, i + 1);
            }
        }
        ret <<= 8;
        ret |= buf[8] as u64;
        (ret, 9)
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
        // TODO: in case buffer length is zero?
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
        // However, practically SQLite does not pass such big value to len_varint(). So
        // They don't think it's a bug. https://sqlite-users.sqlite.narkive.com/J3jNoCns/sqlite-assertion-hit-in-sqlite3varintlen
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
    // checks if the value will take 9 bytes
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
        // sets the last continuation bit to "0"
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

/// Result of parse_integer()
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
///
/// * Contains non-digit characters
/// * Contains no digits
pub fn parse_integer(input: &[u8]) -> (bool, ParseIntegerResult) {
    let mut n = 0;
    // skip leading spaces
    for byte in input {
        if !is_space(*byte) {
            break;
        }
        n += 1;
    }
    // sign
    let is_positive = match input.get(n) {
        Some(b'-') => {
            n += 1;
            false
        },
        Some(b'+') => {
            n += 1;
            false
        },
        _ => true,
    };
    // skip leading zeros
    let mut n_zeros = 0;
    for byte in &input[n..] {
        if *byte != b'0' {
            break;
        }
        n_zeros += 1;
    }
    // parse digits
    let mut int: u64 = 0;
    let mut n_digits = 0;
    for &byte in &input[n + n_zeros..] {
        if byte.is_ascii_digit() {
            int = int.wrapping_mul(10).wrapping_add((byte - b'0') as u64);
            n_digits += 1;
        }
        else {
            break;
        }
    }
    // extra non-digits at the tail
    let mut valid = true;
    for byte in &input[n + n_zeros + n_digits..] {
        if !is_space(*byte) {
            valid = false;
            break;
        }
    }
    if n_zeros + n_digits == 0 {
        (false, ParseIntegerResult::Empty)
    } else if n_digits < 19 {
        let v = if is_positive { int as i64 } else { -(int as i64) };
        (valid, ParseIntegerResult::Integer(v))
    } else if n_digits > 19 || int > MAX_I64_PLUS_ONE {
        (valid, ParseIntegerResult::TooBig(is_positive))
    } else if int == MAX_I64_PLUS_ONE {
        if is_positive {
            (valid, ParseIntegerResult::MaxPlusOne)
        } else {
            (valid, ParseIntegerResult::Integer(i64::MIN))
        }
    } else {
        assert!(int < MAX_I64_PLUS_ONE);
        let v = if is_positive { int as i64 } else { -(int as i64) };
        (valid, ParseIntegerResult::Integer(v))
    }
}

/// Parse float literal.
///
/// This is inspired by `sqlite3AtoF()` of SQLite.
///
/// This ignores leading and tailing spaces.
///
/// The input may:
///
/// * Contain '.'
/// * Contain 'e' or 'E'
/// * Overflow i64 range
pub fn parse_float(input: &[u8]) -> (bool, bool, f64) {
    let mut n = 0;
    let mut pure_integer = false;
    // skip leading spaces
    for byte in input {
        if !is_space(*byte) {
            break;
        }
        n += 1;
    }
    // sign
    let is_positive = match input.get(n) {
        Some(b'-') => {
            n += 1;
            false
        },
        Some(b'+') => {
            n += 1;
            false
        },
        _ => true,
    };
    // skip leading zeros
    let mut n_zeros = 0;
    for byte in &input[n..] {
        if *byte != b'0' {
            break;
        }
        n_zeros += 1;
        pure_integer = true;
    }
    n += n_zeros;
    let mut significand = 0;
    let mut decimal_point: i64 = 0;
    let mut n_digits = 0;
    // parse integer part
    for byte in &input[n..] {
        if byte.is_ascii_digit() {
            significand = significand * 10 + (byte - b'0') as i64;
            n_digits += 1;
            pure_integer = true;
            if significand < (i64::MAX - 9) / 10 {
                continue;
            }
        }
        break;
    }
    n += n_digits;
    for byte in &input[n..] {
        if byte.is_ascii_digit() {
            decimal_point += 1;
            n += 1;
        } else {
            break;
        }
    }
    // parse fractional part
    if let Some(&b'.') = input.get(n) {
        pure_integer = false;
        n += 1;
        let mut fractional_digits = 0;
        for byte in &input[n..] {
            if byte.is_ascii_digit() {
                if significand < (i64::MAX - 9) / 10 {
                    significand = significand * 10 + (byte - b'0') as i64;
                    decimal_point -= 1;
                }
                fractional_digits += 1;
            } else {
                break;
            }
        }
        n_digits += fractional_digits;
        n += fractional_digits;
    }
    // parse exponent part
    let mut is_valid = match input.get(n) {
        Some(b'e') | Some(b'E') => {
            n += 1;
            let exponent_sign = match input.get(n) {
                Some(b'+') => {
                    n += 1;
                    1
                },
                Some(b'-') => {
                    n += 1;
                    -1
                },
                _ => -1,
            };
            let mut exponent = 0;
            let mut is_valid_exponent = false;
            for byte in &input[n..] {
                if byte.is_ascii_digit() {
                    exponent = if exponent < 10000 {
                        exponent * 10 + (byte - b'0') as i32
                    } else {
                        10000
                    };
                    n += 1;
                    is_valid_exponent = true;
                } else {
                    break;
                }
            }
            decimal_point += (exponent * exponent_sign) as i64;
            if is_valid_exponent {
                pure_integer = false;
            }
            is_valid_exponent
        }
        _ => true
    };
    is_valid = is_valid && n_zeros + n_digits > 0;
    // skip leading spaces
    if is_valid {
        for byte in &input[n..] {
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
    (is_valid, pure_integer, v)
}
