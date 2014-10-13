#![feature(if_let)]
#![feature(slicing_syntax)]

//pub use parser::parse;

pub mod lexer;
//pub mod parser;

#[deriving(Clone, Show, PartialEq, Eq)]
pub struct Span {
    pub line: u32,
    pub offset: u32,
}

#[deriving(Clone, Show, PartialEq)]
pub struct Spanned<T> {
    pub content: T,
    pub start: Span,
}
