use crate::utils::{is_space, HexedBytes, MaybeQuotedBytes, UPPER_TO_LOWER};

// 'X' or 'x' (0x58 or 0x78 in ASCII) indicates a hexadecimal blob literal.
// Example: X'48656C6C6F' (blob containing 'Hello').
const CHAR_X: u8 = 0x00;
const CHAR_ALPHABET: u8 = 0x01;
const CHAR_UNDERSCORE: u8 = 0x02;
const CHAR_DIGIT: u8 = 0x03;
const CHAR_DOLLAR: u8 = 0x04;
const CHAR_QUOTE: u8 = 0x05; // '"', '\'', '`'
const CHAR_QUOTE2: u8 = 0x06; // '['
const CHAR_INVALID: u8 = 0xFF;

static CHAR_LOOKUP_TABLE: [u8; 256] = [
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0x00 - 0x07
    0xFF, b' ', b' ', 0xFF, b' ', b' ', 0xFF, 0xFF, // 0x08 - 0x0F
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0x10 - 0x17
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0x18 - 0x1F
    b' ', b'!', 0x05, 0xFF, 0x04, 0xFF, 0xFF, 0x05, // 0x20 - 0x27
    b'(', b')', b'*', b'+', b',', b'-', b'.', 0xFF, // 0x28 - 0x2F
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, // 0x30 - 0x37
    0x03, 0x03, 0xFF, b';', b'<', b'=', b'>', 0xFF, // 0x38 - 0x3F
    0xFF, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, // 0x40 - 0x47
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, // 0x48 - 0x4F
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, // 0x50 - 0x57
    0x00, 0x01, 0x01, 0x06, 0xFF, 0xFF, 0xFF, 0x02, // 0x58 - 0x5F
    0x05, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, // 0x60 - 0x67
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, // 0x68 - 0x6F
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, // 0x70 - 0x77
    0x00, 0x01, 0x01, 0xFF, b'|', 0xFF, b'~', 0xFF, // 0x78 - 0x7F
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0x80 - 0x87
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0x88 - 0x8F
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0x90 - 0x97
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0x98 - 0x9F
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xA0 - 0xA7
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xA8 - 0xAF
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xB0 - 0xB7
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xB8 - 0xBF
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xC0 - 0xC7
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xC8 - 0xCF
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xD0 - 0xD7
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xD8 - 0xDF
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xE0 - 0xE7
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xE8 - 0xEF
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xF0 - 0xF7
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // 0xF8 - 0xFF
];

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    // keywords
    As,
    Cast,
    Collate,
    Create,
    Delete,
    From,
    Index,
    Insert,
    Into,
    Key,
    Null,
    On,
    Primary,
    Select,
    Table,
    Values,
    Where,
    // symbols
    Space,
    LeftParen,
    RightParen,
    Asterisk,
    Plus,
    Minus,
    Comma,
    Dot,
    Semicolon,
    Tilda,
    // operators
    /// equal to
    Eq,
    /// not equal to
    Ne,
    /// greater than
    Gt,
    /// greater than or equal to
    Ge,
    /// less than
    Lt,
    /// less than or equal to
    Le,
    BitOr,
    Concat,
    // literals
    Identifier(MaybeQuotedBytes<'a>),
    String(MaybeQuotedBytes<'a>),
    Blob(HexedBytes<'a>),
    // only contains 0-9 chars
    Integer(&'a [u8]),
    Float(&'a [u8]),
    Illegal,
}

pub fn get_token(input: &[u8]) -> Option<(usize, Token)> {
    if input.is_empty() {
        return None;
    }
    match CHAR_LOOKUP_TABLE[input[0] as usize] {
        b' ' => {
            for (i, &byte) in input.iter().skip(1).enumerate() {
                if !is_space(byte) {
                    return Some((i + 1, Token::Space))
                }
            }
            Some((input.len(), Token::Space))
        },
        b'!' => {
            if input.len() >= 2 && input[1] == b'=' {
                Some((2, Token::Ne))
            } else {
                Some((1, Token::Illegal))
            }
        },
        b'(' => Some((1, Token::LeftParen)),
        b')' => Some((1, Token::RightParen)),
        b'*' => Some((1, Token::Asterisk)),
        b'+' => Some((1, Token::Plus)),
        b'-' => Some((1, Token::Minus)),
        b',' => Some((1, Token::Comma)),
        b'.' => {
            if input.len() >= 2 && input[1].is_ascii_digit() {
                let (len, is_valid) = decimal_len(input);
                if is_valid {
                    Some((len, Token::Float(&input[..len])))
                } else {
                    Some((len, Token::Illegal))
                }
            } else {
                Some((1, Token::Dot))
            }
        },
        b';' => Some((1, Token::Semicolon)),
        b'<' => {
            if input.len() >= 2 {
                match input[1] {
                    b'=' => Some((2, Token::Le)),
                    // `<>` is interchangeable with `!=`
                    b'>' => Some((2, Token::Ne)),
                    _ => Some((1, Token::Lt)),
                }
            } else {
                Some((1, Token::Lt))
            }
        },
        b'=' => {
            // `=` and `==` are the same thing
            if input.len() >= 2 && input[1] == b'=' {
                Some((2, Token::Eq))
            } else {
                Some((1, Token::Eq))
            }
        },
        b'>' => {
            if input.len() >= 2 && input[1] == b'=' {
                Some((2, Token::Ge))
            } else {
                Some((1, Token::Gt))
            }
        },
        b'|' => {
            if input.len() >= 2 && input[1] == b'|' {
                // `||` is string concatenation
                Some((2, Token::Concat))
            } else {
                Some((1, Token::BitOr))
            }
        },
        b'~' => Some((1, Token::Tilda)),
        CHAR_X => {
            if input.len() >= 2 && input[1] == b'\'' {
                let mut iter = input.iter().skip(2).enumerate();
                for (i, byte) in iter.by_ref() {
                    if byte.is_ascii_hexdigit() {
                        continue;
                    } else if *byte == b'\'' {
                        // 3 = 'X | x' + '\'' + 1 (plus one because first iteration i = 0)
                        let len = i + 3;
                        // closing '\'' makes even `i` valid
                        if i % 2 == 0 {
                            return Some((len, Token::Blob((&input[2..i + 2]).into())));
                        } else {
                            return Some((len, Token::Illegal));
                        }
                    } else {
                        break;
                    }
                }
                // Vacuum invalid hex digits.
                for (i, byte) in iter {
                    if *byte == b'\'' {
                        return Some((i + 3, Token::Illegal))
                    }
                }
                Some((input.len(), Token::Illegal))
            } else {
                let len = identifier_len(input);
                let ident = &input[..len];
                Some((len, Token::Identifier(ident.into())))
            }
        },
        CHAR_ALPHABET | CHAR_UNDERSCORE => {
            let len = identifier_len(input);
            let ident = &input[..len];
            const MAX_KEYWORD_LEN: usize = 7;
            if len <= MAX_KEYWORD_LEN {
                let mut lower_ident = [0; MAX_KEYWORD_LEN];
                for (i, &byte) in ident.iter().take(MAX_KEYWORD_LEN).enumerate() {
                    lower_ident[i] = UPPER_TO_LOWER[byte as usize];
                };
                // zeroes are empty bytes
                match &lower_ident {
                    b"as\0\0\0\0\0" => Some((len, Token::As)),
                    b"cast\0\0\0" => Some((len, Token::Cast)),
                    b"collate" => Some((len, Token::Collate)),
                    b"create\0" => Some((len, Token::Create)),
                    b"delete\0" => Some((len, Token::Delete)),
                    b"from\0\0\0" => Some((len, Token::From)),
                    b"index\0\0" => Some((len, Token::Index)),
                    b"insert\0" => Some((len, Token::Insert)),
                    b"into\0\0\0" => Some((len, Token::Into)),
                    b"key\0\0\0\0" => Some((len, Token::Key)),
                    b"null\0\0\0" => Some((len, Token::Null)),
                    b"on\0\0\0\0\0" => Some((len, Token::On)),
                    b"primary" => Some((len, Token::Primary)),
                    b"select\0" => Some((len, Token::Select)),
                    b"table\0\0" => Some((len, Token::Table)),
                    b"values\0" => Some((len, Token::Values)),
                    b"where\0\0" => Some((len, Token::Where)),
                    _ => Some((len, Token::Identifier(ident.into()))),
                }
            } else {
                Some((len, Token::Identifier(ident.into())))
            }
        },
        CHAR_DIGIT => {
            let mut len = 1;
            for &byte in input.iter().skip(len) {
                // NOTE: u8::is_ascii_digit() is faster than CHAR_LOOKUP_TABLE.
                if byte.is_ascii_digit() {
                    len += 1;
                } else {
                    break;
                }
            }
            let (d_len, is_valid) = decimal_len(&input[len..]);
            if !is_valid {
                Some((len + d_len, Token::Illegal))
            } else if d_len == 0 {
                Some((len, Token::Integer(&input[..len])))
            } else {
                Some((len + d_len, Token::Float(&input[..len + d_len])))
            }
        },
        // '"', '\'', '`'
        CHAR_QUOTE => {
            let delim = input[0];
            let mut iter = input.iter().skip(1).enumerate();
            while let Some((i, &byte)) = iter.next() {
                // search for closing delimiter
                if byte == delim {
                    if let Some((_, &byte)) = iter.next() {
                        // if a delimiter is followed by another delimiter
                        // it's an escaped delimiter (literal character).
                        if byte == delim {
                            continue;
                        }
                    }
                    let quoted_bytes = &input[..i + 1];
                    if delim == b'\'' {
                        return Some((i + 1, Token::String(quoted_bytes.into())))
                    } else {
                        return Some((i + 1, Token::Identifier(quoted_bytes.into())))
                    }
                }
            }
            Some((input.len(), Token::Illegal))
        },
        // '['
        CHAR_QUOTE2 => {
            for (i, &byte) in input.iter().skip(1).enumerate() {
                if byte == b']' {
                    let unquoted_bytes = &input[1..i];
                    return Some((i + 1, Token::Identifier(unquoted_bytes.into())))
                }
            }
            Some((input.len(), Token::Illegal))
        },
        c => {
            unreachable!("unexpected char code: ({}), char: {}", c, input[0]);
        },
    }
}

fn decimal_len(input: &[u8]) -> (usize, bool) {
    let mut len = 0;
    // NOTE: Empty string and just a dot are handled in `get_token` function.
    if !input.is_empty() && input[0] == b'.' {
        len += 1;
        for &byte in input.iter().skip(len) {
            if byte.is_ascii_digit() {
                len += 1;
            } else {
                break;
            }
        }
    }
    if input.len() > len && (input[len] == b'e' || input[len] == b'E') {
        len += 1;
        let mut iter = input.iter().skip(len);
        let Some(&byte) = iter.next() else {
            return (len, false);
        };
        let mut byte = byte;
        if byte == b'+' || byte == b'-' {
            len += 1;
            if let Some(&b) = iter.next() {
                byte = b;
            } else {
                // NOTE: it's a null byte in ASCII not a digit "0".
                byte = 0;
            };
        };
        if byte.is_ascii_digit() {
            len += 1;
        } else {
            return (len, false);
        }
        for &byte in iter {
            if byte.is_ascii_digit() {
                len += 1;
            } else {
                break;
            }
        }
    }
    (len, true)
}

fn identifier_len(input: &[u8]) -> usize {
    // Skip the first byte since it is already checked.
    for (i, &byte) in input.iter().skip(1).enumerate() {
        // CHAR_X (0x00), CHAR_ALPHABET (0x01), CHAR_UNDERSCORE (0x02),
        // CHAR_DIGIT (0x03) are less than CHAR_DOLLAR (0x04) and
        // they are the valid identifier characters.
        if CHAR_LOOKUP_TABLE[byte as usize] > CHAR_DOLLAR {
            return i + 1;
        }
    }
    input.len()
}