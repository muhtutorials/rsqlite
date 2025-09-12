/// Data type affinity.
///
/// https://www.sqlite.org/datatype3.html#type_affinity
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TypeAffinity {
    Text,
    Numeric,
    Integer,
    Real,
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
