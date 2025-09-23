use crate::token::{Token, get_token};
use crate::utils::{HexedBytes, MaybeQuotedBytes, ParseIntegerResult, parse_float, parse_integer};
use std::fmt::{Display, Formatter};
use std::str::from_utf8;

#[derive(Debug)]
pub struct Error<'a> {
    input: &'a [u8],
    cursor: usize,
    token_len: usize,
    msg: &'static str,
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Ok(sql) = from_utf8(self.input) else {
            return Err(std::fmt::Error);
        };
        let mut n_chars = 0;
        let mut pos = self.cursor;
        for c in sql.chars() {
            if c.len_utf8() > pos {
                break;
            }
            pos -= c.len_utf8();
            n_chars += 1;
        }
        let token_size = if self.token_len > 0 {
            self.token_len
        } else {
            1
        };
        write!(
            f,
            "{}\n{}\n{}{}",
            self.msg,
            sql,
            " ".repeat(n_chars),
            "^".repeat(token_size)
        )
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

static NULL_BYTES: &[u8] = b"null";

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    input: &'a [u8],
    cursor: usize,
    token_len: usize,
    token: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        let mut parser = Self {
            input,
            cursor: 0,
            token_len: 0,
            token: None,
        };
        parser.next();
        parser
    }

    fn next<'b>(&'b mut self) -> Option<&'b Token<'a>> {
        self.cursor += self.token_len;
        if let Some((len, token)) = get_token(&self.input[self.cursor..]) {
            self.token_len = len;
            if token == Token::Space {
                return self.next();
            }
            self.token = Some(token);
            self.token.as_ref()
        } else {
            self.token_len = 0;
            self.token = None;
            None
        }
    }

    fn peek<'b>(&'b mut self) -> Option<&'b Token<'a>> {
        self.token.as_ref()
    }

    fn error(&self, msg: &'static str) -> Error<'a> {
        Error {
            input: self.input,
            cursor: self.cursor,
            token_len: self.token_len,
            msg,
        }
    }
}

pub enum Stmt<'a> {
    Select(Select<'a>),
    Insert(Insert<'a>),
    Delete(Delete<'a>),
}

pub fn parse_sql<'a>(p: &mut Parser<'a>) -> Result<'a, Stmt<'a>> {
    match p.peek() {
        Some(Token::Select) => {
            let select = parse_select(p)?;
            Ok(Stmt::Select(select))
        }
        Some(Token::Insert) => {
            let insert = parse_insert(p)?;
            Ok(Stmt::Insert(insert))
        }
        Some(Token::Delete) => {
            let delete = parse_delete(p)?;
            Ok(Stmt::Delete(delete))
        }
        _ => Err(p.error("no statement")),
    }
}

/// Assert that the next token is a semicolon.
pub fn expect_semicolon<'a>(p: &mut Parser<'a>) -> Result<'a, ()> {
    match p.peek() {
        Some(Token::Semicolon) => {}
        _ => return Err(p.error("no semicolon")),
    };
    p.next();
    Ok(())
}

/// Assert that there is no token except spaces.
///
/// Uses mutable [Parser] to unify the interface with other `expect` functions.
pub fn expect_no_more_tokens<'a>(p: &mut Parser<'a>) -> Result<'a, ()> {
    match p.peek() {
        Some(_) => Err(p.error("unexpected token")),
        _ => Ok(()),
    }
}

/// CREATE TABLE statement.
#[derive(Debug, PartialEq, Eq)]
pub struct CreateTable<'a> {
    pub table_name: MaybeQuotedBytes<'a>,
    pub columns: Vec<ColumnDef<'a>>,
}

/// Parse CREATE TABLE statement.
///
/// https://www.sqlite.org/lang_createtable.html
pub fn parse_create_table<'a>(p: &mut Parser<'a>) -> Result<'a, CreateTable> {
    // CREATE TABLE large_table (
    //     id BIGINT PRIMARY KEY,
    //     category VARCHAR(50) NOT NULL,
    //     value DECIMAL(15,6),
    //     created_date DATE NOT NULL,
    // );
    let Some(Token::Create) = p.peek() else {
        return Err(p.error("no create"));
    };
    let Some(Token::Table) = p.next() else {
        return Err(p.error("no table"));
    };
    let Some(Token::Identifier(table_name)) = p.next() else {
        return Err(p.error("no table name"));
    };
    let table_name = *table_name;
    let Some(Token::LeftParen) = p.next() else {
        return Err(p.error("no left parenthesis"));
    };
    let mut columns = Vec::new();
    loop {
        // parse ColumnDef
        let Some(Token::Identifier(name)) = p.next() else {
            return Err(p.error("no column name"));
        };
        let name = *name;
        p.next();
        let type_name = parse_type_name(p)?;
        let mut constraints = Vec::new();
        while let Some(constraint) = parse_column_constraint(p)? {
            constraints.push(constraint);
        }
        columns.push(ColumnDef {
            name,
            type_name,
            constraints,
        });
        // Parser contains a peekable token after `parse_column_constraint`.
        match p.peek() {
            Some(Token::Comma) => continue,
            Some(Token::RightParen) => break,
            _ => return Err(p.error("no right parenthesis")),
        }
    }
    p.next();
    Ok(CreateTable {
        table_name,
        columns,
    })
}

/// https://www.sqlite.org/syntax/type-name.html
pub fn parse_type_name<'a>(p: &mut Parser<'a>) -> Result<'a, Vec<MaybeQuotedBytes<'a>>> {
    let mut type_name = Vec::new();
    match p.peek() {
        Some(Token::Null) => {
            type_name.push(NULL_BYTES.into());
        }
        Some(Token::Identifier(ident)) => {
            type_name.push(*ident);
        }
        _ => return Ok(Vec::new()),
    }
    loop {
        match p.next() {
            Some(Token::Null) => {
                type_name.push(NULL_BYTES.into());
            }
            Some(Token::Identifier(ident)) => {
                type_name.push(*ident);
            }
            Some(Token::LeftParen) => {
                p.next();
                // Just check whether signed numbers are valid and move cursor without
                // parsing the number. Signed numbers in a type name has no meanings to type
                // affinity.
                // https://www.sqlite.org/datatype3.html#affinity_name_examples
                // Parse signed numbers.
                skip_signed_number(p)?;
                if p.peek() == Some(&Token::Comma) {
                    p.next();
                    skip_signed_number(p)?;
                };
                if p.peek() != Some(&Token::RightParen) {
                    return Err(p.error("type name: no right paren"));
                }
                p.next();
                break;
            }
            _ => break,
        };
    }
    Ok(type_name)
}

/// https://www.sqlite.org/syntax/signed-number.html
fn skip_signed_number<'a>(p: &mut Parser<'a>) -> Result<'a, ()> {
    if matches!(p.peek(), Some(Token::Plus | Some(Token::Minus))) {
        p.next();
    };
    if !matches!(p.peek(), Some(Token::Integer(_) | Some(Token::Float(_)))) {
        return Err(p.error("no signed number"));
    };
    p.next();
    Ok(())
}

/// Constraint of a column in a table.
#[derive(Debug, PartialEq, Eq)]
pub enum ColumnConstraint<'a> {
    Collate(MaybeQuotedBytes<'a>),
    PrimaryKey,
}

/// https://www.sqlite.org/syntax/column-constraint.html
fn parse_column_constraint<'a>(p: &mut Parser<'a>) -> Result<'a, Option<ColumnConstraint<'a>>> {
    match p.peek() {
        Some(Token::Collate) => {
            let Some(Token::Identifier(collation)) = p.next() else {
                return Err(p.error("no collation name"));
            };
            let collation = *collation;
            p.next();
            Ok(Some(ColumnConstraint::Collate(collation)))
        }
        Some(Token::Primary) => {
            let Some(Token::Key) = p.next() else {
                return Err(p.error("no key primary"));
            };
            p.next();
            Ok(Some(ColumnConstraint::PrimaryKey))
        }
        _ => Ok(None),
    }
}

/// Definition of a column in a table.
#[derive(Debug, PartialEq, Eq)]
pub struct ColumnDef<'a> {
    pub name: MaybeQuotedBytes<'a>,
    pub type_name: Vec<MaybeQuotedBytes<'a>>,
    pub constraints: Vec<ColumnConstraint<'a>>,
}

#[derive(Debug)]
pub struct Select<'a> {
    pub table_name: MaybeQuotedBytes<'a>,
    pub columns: Vec<ResultColumn<'a>>,
    pub filter: Option<Expr<'a>>,
}

// Parse SELECT statement.
//
// https://www.sqlite.org/lang_select.html
pub fn parse_select<'a>(p: &mut Parser<'a>) -> Result<'a, Select<'a>> {
    let Some(Token::Select) = p.peek() else {
        return Err(p.error("no select"));
    };
    p.next();
    let result_column = parse_result_column(p)?;
    todo!()
}

#[derive(Debug, PartialEq)]
pub struct Insert<'a> {
    pub table_name: MaybeQuotedBytes<'a>,
    pub columns: Vec<ResultColumn<'a>>,
    pub value: Vec<Vec<Expr<'a>>>,
}

#[derive(Debug)]
pub struct Delete<'a> {
    pub table_name: MaybeQuotedBytes<'a>,
    pub filter: Option<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum ResultColumn<'a> {
    All,
    AllOfTable(MaybeQuotedBytes<'a>),
    Expr((Expr<'a>, Option<MaybeQuotedBytes<'a>>)),
}

/// Parse result column.
///
/// https://www.sqlite.org/syntax/result-column.html
pub fn parse_result_column<'a>(p: &mut Parser<'a>) -> Result<'a, ResultColumn<'a>> {
    match p.peek() {
        Some(Token::Identifier(table_name)) => {
            let table_name = *table_name;
            let mut cloned_parser = p.clone();
            if Some(&Token::Dot) == cloned_parser.next()
                && Some(&Token::Asterisk) == cloned_parser.next()
            {
                cloned_parser.next();
                *p = cloned_parser;
                return Ok(ResultColumn::AllOfTable(table_name));
            };
        }
        Some(Token::Asterisk) => {
            p.next();
            return Ok(ResultColumn::All);
        }
        _ => {}
    }
    let expr = parse_expr(p)?;
    todo!()
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    BitNot,
    Minus,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    Compare(CompareOp),
    Concat,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CompareOp {
    Eq, // =
    Ne, // !=
    Gt, // >
    Ge, // >=
    Lt, // <
    Le, // <=
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Column(MaybeQuotedBytes<'a>),
    Collate {
        expr: Box<Expr<'a>>,
        collation_name: MaybeQuotedBytes<'a>,
    },
    Cast {
        expr: Box<Expr<'a>>,
        type_name: Vec<MaybeQuotedBytes<'a>>,
    },
    UnaryOperator {
        expr: Box<Expr<'a>>,
        operator: UnaryOp,
    },
    BinaryOperator {
        operator: BinaryOp,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    Integer(i64),
    Real(f64),
    Text(MaybeQuotedBytes<'a>),
    Blob(HexedBytes<'a>),
    Null,
}

/// Parse expression.
///
/// https://www.sqlite.org/syntax/expr.html
pub fn parse_expr<'a>(p: &mut Parser<'a>) -> Result<'a, Expr<'a>> {
    parse_expr_eq(p)
}

pub fn parse_expr_eq<'a>(p: &mut Parser<'a>) -> Result<'a, Expr<'a>> {
    let mut expr = parse_expr_compare(p)?;
    todo!()
}

pub fn parse_expr_compare<'a>(p: &mut Parser<'a>) -> Result<'a, Expr<'a>> {
    let mut expr = parse_expr_concat(p)?;
    todo!()
}

pub fn parse_expr_concat<'a>(p: &mut Parser<'a>) -> Result<'a, Expr<'a>> {
    let mut expr = parse_expr_collate(p)?;
    todo!()
}

pub fn parse_expr_collate<'a>(p: &mut Parser<'a>) -> Result<'a, Expr<'a>> {
    let mut expr = parse_expr_unary(p)?;
    todo!()
}

pub fn parse_expr_unary<'a>(p: &mut Parser<'a>) -> Result<'a, Expr<'a>> {
    match p.peek() {
        Some(Token::Plus) => {
            p.next();
            // Unary operator `+` is a no-op.
            let expr = parse_expr_unary(p)?;
            Ok(expr)
        }
        Some(Token::Minus) => match p.next() {
            Some(Token::Integer(buf)) => {
                let (is_valid, parsed_int) = parse_integer(buf);
                assert!(is_valid);
                let expr = match parsed_int {
                    ParseIntegerResult::Integer(v) => Expr::Integer(-v),
                    ParseIntegerResult::MaxPlusOne => Expr::Integer(i64::MIN),
                    ParseIntegerResult::TooBig(_) => {
                        let (is_valid, is_integer, f) = parse_float(buf);
                        assert!(is_valid);
                        assert!(is_integer);
                        Expr::Real(-f)
                    }
                    ParseIntegerResult::Empty => {
                        unreachable!("token integer must contain at least one digit")
                    }
                };
                p.next();
                Ok(expr)
            }
            Some(Token::Float(buf)) => {
                let (is_valid, is_integer, f) = parse_float(buf);
                assert!(is_valid);
                assert!(is_integer);
                p.next();
                Ok(Expr::Real(-f))
            }
            _ => {
                let expr = parse_expr_unary(p)?;
                Ok(Expr::UnaryOperator {
                    operator: UnaryOp::Minus,
                    expr: Box::new(expr),
                })
            }
        },
        _ => parse_expr_primitive(p),
    }
}

pub fn parse_expr_primitive<'a>(p: &mut Parser<'a>) -> Result<'a, Expr<'a>> {
    let expr = match p.peek() {
        Some(Token::Identifier(ident)) => Expr::Column(*ident),
        Some(Token::Cast) => {
            let Some(Token::LeftParen) = p.next() else {
                return Err(p.error("no cast left parenthesis"));
            };
            p.next();
            let expr = parse_expr(p)?;
            let Some(Token::As) = p.peek() else {
                return Err(p.error("no cast as"));
            };
            p.next();
            let type_name = parse_type_name(p)?;
            let Some(Token::RightParen) = p.peek() else {
                return Err(p.error("no cast right parenthesis"));
            };
            Expr::Cast {
                expr: Box::new(expr),
                type_name,
            }
        }
        Some(Token::Integer(buf)) => {
            let (is_valid, parsed_int) = parse_integer(buf);
            assert!(is_valid);
            match parsed_int {
                ParseIntegerResult::Integer(v) => Expr::Integer(v),
                ParseIntegerResult::MaxPlusOne | ParseIntegerResult::TooBig(_) => {
                    let (is_valid, is_integer, f) = parse_float(buf);
                    assert!(is_valid);
                    assert!(is_integer);
                    Expr::Real(f)
                }
                ParseIntegerResult::Empty => {
                    unreachable!("token integer must contain at least one digit")
                }
            }
        }
        Some(Token::Float(buf)) => {
            let (is_valid, is_integer, f) = parse_float(buf);
            assert!(is_valid);
            assert!(is_integer);
            Expr::Real(f)
        }
        Some(Token::String(text)) => Expr::Text(*text),
        Some(Token::Blob(hex)) => Expr::Blob(*hex),
        Some(Token::Null) => Expr::Null,
        _ => return Err(p.error("no expr")),
    };
    p.next();
    Ok(expr)
}
