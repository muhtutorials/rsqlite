use crate::utils::{CaseInsensitiveBytes, ParseIntegerResult, parse_float, parse_integer};
use std::cmp::Ordering;
use std::io::Write;
use std::ops::Deref;

/// Data type affinity.
///
/// https://www.sqlite.org/datatype3.html#type_affinity
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TypeAffinity {
    Numeric,
    Integer,
    Real,
    Text,
    Blob,
}

/// The default collation sequence is "binary".
pub static DEFAULT_COLLATION: Collation = Collation::Binary;

/// Types of text comparison and sorting.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Collation {
    // regular
    Binary,
    // case-insensitive
    NoCase,
    // as binary but ignores trailing spaces
    RTrim,
}

#[derive(Debug, Clone)]
pub enum Buffer<'a> {
    Owned(Vec<u8>),
    Borrowed(&'a [u8]),
}

impl Buffer<'_> {
    pub fn into_vec(self) -> Vec<u8> {
        match self {
            Buffer::Owned(buf) => buf,
            Buffer::Borrowed(buf) => buf.to_vec(),
        }
    }
}

impl<'a> Deref for Buffer<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            Buffer::Owned(buf) => buf,
            Buffer::Borrowed(buf) => buf,
        }
    }
}

impl PartialEq for Buffer<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl<'a> From<Vec<u8>> for Buffer<'a> {
    fn from(value: Vec<u8>) -> Self {
        Self::Owned(value)
    }
}

impl<'a> From<&'a [u8]> for Buffer<'a> {
    fn from(value: &'a [u8]) -> Self {
        Self::Borrowed(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Integer(i64),
    Real(f64),
    // NOTE: Any text is not guaranteed to be valid UTF-8.
    Text(Buffer<'a>),
    Blob(Buffer<'a>),
}

impl<'a> Value<'a> {
    pub fn display<W: Write>(&self, w: &mut W) -> std::io::Result<()> {
        match self {
            Value::Integer(i) => write!(w, "{i}"),
            Value::Real(f) => write!(w, "{f}"),
            Value::Text(buf) => w.write_all(buf),
            Value::Blob(buf) => w.write_all(buf),
        }
    }

    /// Apply [TypeAffinity] to the value.
    ///
    /// Applying type affinity happens when storing a value to database.
    ///
    /// https://www.sqlite.org/datatype3.html#type_affinity
    pub fn apply_affinity(self, type_affinity: TypeAffinity) -> Self {
        match type_affinity {
            TypeAffinity::Numeric | TypeAffinity::Integer => self.apply_numeric_affinity(),
            TypeAffinity::Real => match self {
                Value::Integer(i) => Value::Real(i as f64),
                Value::Real(f) => Value::Real(f),
                Value::Text(buf) => {
                    let (valid, _, f) = parse_float(&buf);
                    if valid {
                        Value::Real(f)
                    } else {
                        Value::Text(buf)
                    }
                }
                Value::Blob(buf) => Value::Blob(buf),
            },
            TypeAffinity::Text => self.apply_text_affinity(),
            TypeAffinity::Blob => self,
        }
    }

    /// Convert the text value to a numeric value if it is well-formed.
    /// Otherwise, return the original value.
    pub fn apply_numeric_affinity(self) -> Self {
        match self {
            Value::Integer(i) => Value::Integer(i),
            Value::Real(f) => {
                let fi = real_to_int(f);
                if is_real_same_as_int(f, fi) {
                    Value::Integer(fi)
                } else {
                    Value::Real(f)
                }
            }
            Value::Text(buf) => match parse_integer(&buf) {
                (true, ParseIntegerResult::Integer(i)) => Value::Integer(i),
                _ => {
                    let (valid, _, f) = parse_float(&buf);
                    if valid {
                        let fi = real_to_int(f);
                        if is_real_same_as_int(f, fi) {
                            Value::Integer(fi)
                        } else {
                            Value::Real(f)
                        }
                    } else {
                        Value::Text(buf)
                    }
                }
            },
            Value::Blob(buf) => Value::Blob(buf),
        }
    }

    /// Convert the value to a text value.
    ///
    /// For [Value::Text] and [Value::Blob] values, this just changes the type
    /// of the value.
    ///
    /// For [Value::Integer] and [Value::Real] values, this
    /// converts the value to a text value.
    pub fn apply_text_affinity(self) -> Self {
        match self {
            Value::Integer(i) => {
                // i64 is at most 19 digits. 1 byte is for sign (-).
                let mut text_buf = Vec::with_capacity(20);
                write!(text_buf, "{i}").unwrap();
                Value::Text(Buffer::Owned(text_buf))
            }
            Value::Real(f) => {
                let mut text_buf = Vec::new();
                write!(text_buf, "{f}").unwrap();
                Value::Text(Buffer::Owned(text_buf))
            }
            Value::Text(buf) => Value::Text(buf),
            Value::Blob(buf) => Value::Blob(buf),
        }
    }

    /// Convert the value to an integer value if it is well-formed. Otherwise,
    /// convert it to 0.
    pub fn as_integer(&self) -> i64 {
        match self {
            Value::Integer(i) => *i,
            Value::Real(f) => real_to_int(*f),
            Value::Text(buf) | Value::Blob(buf) => {
                let (_, parsed_int) = parse_integer(buf);
                match parsed_int {
                    ParseIntegerResult::Integer(i) => i,
                    ParseIntegerResult::Empty => 0,
                    ParseIntegerResult::MaxPlusOne | ParseIntegerResult::TooBig(true) => i64::MAX,
                    ParseIntegerResult::TooBig(false) => i64::MIN,
                }
            }
        }
    }

    /// Convert the value to text and return the [Buffer].
    pub fn force_text_buffer(self) -> Buffer<'a> {
        match self {
            Value::Integer(i) => {
                // i64 is at most 19 digits. 1 byte is for sign (-).
                let mut text_buf = Vec::with_capacity(20);
                write!(text_buf, "{i}").unwrap();
                Buffer::Owned(text_buf)
            }
            Value::Real(f) => {
                let mut text_buf = Vec::new();
                write!(text_buf, "{f}").unwrap();
                Buffer::Owned(text_buf)
            }
            Value::Text(buf) => buf,
            Value::Blob(buf) => buf,
        }
    }

    /// Convert the [Value] to the type of [TypeAffinity] even if the conversion
    /// is lossy.
    ///
    /// This is used for the CAST expression.
    ///
    /// https://www.sqlite.org/lang_expr.html#castexpr
    pub fn force_apply_type_affinity(self, type_affinity: TypeAffinity) -> Self {
        match type_affinity {
            TypeAffinity::Numeric => match self {
                Value::Integer(i) => Value::Integer(i),
                Value::Real(f) => Value::Real(f),
                Value::Text(buf) | Value::Blob(buf) => {
                    let (_, is_integer, f) = parse_float(&buf);
                    let mut v = if is_integer {
                        let (_, parsed_int) = parse_integer(&buf);
                        match parsed_int {
                            ParseIntegerResult::Integer(i) => Value::Integer(i),
                            _ => Value::Real(f),
                        }
                    } else {
                        Value::Real(f)
                    };
                    if let Value::Real(f) = &v {
                        let fi = real_to_int(*f);
                        if is_real_same_as_int(*f, fi) {
                            v = Value::Integer(fi);
                        }
                    }
                    v
                }
            },
            TypeAffinity::Integer => Value::Integer(self.as_integer()),
            TypeAffinity::Real => match self {
                Value::Integer(i) => Value::Real(i as f64),
                Value::Real(f) => Value::Real(f),
                Value::Text(buf) | Value::Blob(buf) => {
                    let (_, _, f) = parse_float(&buf);
                    Value::Real(f)
                }
            },
            TypeAffinity::Text | TypeAffinity::Blob => {
                let value_type = if type_affinity == TypeAffinity::Text {
                    Value::Text
                } else {
                    Value::Blob
                };
                value_type(self.force_text_buffer())
            }
        }
    }
}

/// doubleToInt64() in vdbemem.c of SQLite.
fn real_to_int(f: f64) -> i64 {
    if f >= i64::MAX as f64 {
        i64::MAX
    } else if f <= i64::MIN as f64 {
        i64::MIN
    } else {
        f as i64
    }
}

/// sqlite3RealSameAsInt() in vdbemem.c of SQLite
fn is_real_same_as_int(f: f64, i: i64) -> bool {
    let fi = i as f64;
    // TODO: Why this range -2251799813685248..2251799813685248 = -2^51..2^51?
    // Mantissa (significand) includes -2^53..2^53 (53 bits, 52 explicitly stored).
    f == 0.0
        || (f.to_ne_bytes() == fi.to_ne_bytes()
            && (-2251799813685248..2251799813685248).contains(&i))
}

#[derive(Debug)]
pub struct ValueCmp<'a>((&'a Value<'a>, &'a Collation));

impl<'a> ValueCmp<'a> {
    pub fn new(value: &'a Value, collation: &'a Collation) -> Self {
        Self((value, collation))
    }

    /// Compare two values.
    ///
    /// https://www.sqlite.org/datatype3.html#comparison_expressions
    /// https://www.sqlite.org/datatype3.html#collating_sequences
    pub fn compare(&self, value: &Value) -> Ordering {
        let (left, collation) = self.0;
        match (left, value) {
            (Value::Integer(i1), Value::Integer(i2)) => i1.cmp(i2),
            (Value::Integer(i1), Value::Real(f2)) => cmp_int_real(*i1, *f2),
            (Value::Real(f1), Value::Integer(i2)) => cmp_int_real(*i2, *f1).reverse(),
            (Value::Real(f1), Value::Real(f2)) => f1.partial_cmp(f2).unwrap(),
            (Value::Integer(_), _) => Ordering::Less,
            (_, Value::Integer(_)) => Ordering::Greater,
            (Value::Real(_), _) => Ordering::Less,
            (_, Value::Real(_)) => Ordering::Greater,
            (Value::Text(t1), Value::Text(t2)) => match collation {
                Collation::Binary => t1.cmp(t2),
                Collation::NoCase => {
                    // TODO: Try passing just t1 and t2.
                    CaseInsensitiveBytes::from(&**t1).cmp(&CaseInsensitiveBytes::from(&**t1))
                }
                Collation::RTrim => {
                    let mut t1_tail = 0;
                    //   0    1    2    3    4
                    // ['h', 'e', 'y', ' ', ' ']
                    // stops at 2, and then we add 1 and get 3,
                    // and thus 0..3 range for comparison
                    for (i, byte) in t1.iter().enumerate().rev() {
                        if *byte != b' ' {
                            t1_tail = i + 1;
                            break;
                        }
                    }
                    let mut t2_tail = 0;
                    for (i, byte) in t2.iter().enumerate().rev() {
                        if *byte != b' ' {
                            t2_tail = i + 1;
                            break;
                        }
                    }
                    t1[..t1_tail].cmp(&t2[..t2_tail])
                }
            },
            (Value::Text(_), Value::Blob(_)) => Ordering::Less,
            (Value::Blob(_), Value::Text(_)) => Ordering::Greater,
            (Value::Blob(b1), Value::Blob(b2)) => b1.cmp(b2),
        }
    }
}

/// Compare i64 and f64.
///
/// This comes from sqlite3IntFloatCompare().
fn cmp_int_real(i: i64, f: f64) -> Ordering {
    if f < -9223372036854775808.0 {
        return Ordering::Greater;
    } else if f >= 9223372036854775808.0 {
        return Ordering::Less;
    }
    // Reasons for usage of this block and not just the second one (fractional precision check):
    // - Integer comparison is exact and fast.
    // - Avoids floating-point conversion for most cases.
    // - Handles edge cases where i as f64 might lose precision for very large integers.
    match i.cmp(&(f as i64)) {
        Ordering::Less => return Ordering::Less,
        Ordering::Greater => return Ordering::Greater,
        Ordering::Equal => {}
    }
    // fractional precision check
    let int_f = i as f64;
    if int_f < f {
        return Ordering::Less;
    } else if int_f > f {
        return Ordering::Greater;
    }
    Ordering::Equal
}

#[derive(Debug, Clone)]
pub enum ConstantValue {
    Integer(i64),
    Real(f64),
    Text(Vec<u8>),
    Blob(Vec<u8>),
}

impl ConstantValue {
    pub fn copy_from(value: Value) -> Self {
        match value {
            Value::Integer(i) => Self::Integer(i),
            Value::Real(f) => Self::Real(f),
            Value::Text(buf) => Self::Text(buf.into_vec()),
            Value::Blob(buf) => Self::Blob(buf.into_vec()),
        }
    }

    pub fn as_value(&self) -> Value {
        match self {
            Self::Integer(i) => Value::Integer(*i),
            Self::Real(f) => Value::Real(*f),
            Self::Text(text) => Value::Text(text.as_slice().into()),
            Self::Blob(blob) => Value::Blob(blob.as_slice().into()),
        }
    }
}
