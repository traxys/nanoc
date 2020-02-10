use logos::Logos;

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub nanolang);

#[derive(Logos, Debug, Copy, Clone)]
enum TokenKind {
    #[end]
    End,
    #[error]
    Error,
    #[regex = "(-)?[0-9]+"]
    Int,
    #[token = "_"]
    Placeholder,
    #[token = "("]
    LParen,
    #[token = ")"]
    RParen,
    #[token = ","]
    Comma,
    #[token = "->"]
    Arrow,
    #[token = "="]
    Equals,
    #[regex = "[a-zA-Z][a-zA-Z_]*"]
    Ident,
    #[token = "{"]
    LBrace,
    #[token = "}"]
    RBrace,
    #[token = ";"]
    Semicolon,
}

#[derive(Debug, Clone, Copy)]
pub enum Token<'i> {
    Int(i64),
    Placeholder,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Arrow,
    Equals,
    Ident(&'i str),
    Semicolon,
}
impl<'i> Token<'i> {
    fn from_kind(kind: TokenKind, input: &'i str) -> Self {
        match kind {
            TokenKind::Semicolon => Self::Semicolon,
            TokenKind::Ident => Self::Ident(input),
            TokenKind::Equals => Self::Equals,
            TokenKind::Arrow => Self::Arrow,
            TokenKind::Comma => Self::Comma,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RParen => Self::RParen,
            TokenKind::LParen => Self::LParen,
            TokenKind::Placeholder => Self::Placeholder,
            TokenKind::Int => Self::Int(input.parse().expect("int is not an int")),
            invalid_kind => panic!("kind can't become a token: {:?}", invalid_kind),
        }
    }
}

pub struct Lexer<'i> {
    lexer: logos::Lexer<TokenKind, &'i str>,
}
impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            lexer: TokenKind::lexer(input),
        }
    }
}

#[derive(Debug)]
pub struct LexerError<'i> {
    text: &'i str,
    start: usize,
    end: usize,
}

pub type Spanned<T, Loc, Error> = Result<(Loc, T, Loc), Error>;

impl<'i> Iterator for Lexer<'i> {
    type Item = Spanned<Token<'i>, usize, LexerError<'i>>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = match self.lexer.token {
            TokenKind::End => None,
            token => {
                let std::ops::Range { start, end, .. } = self.lexer.range();
                Some(match token {
                    TokenKind::Error => Err(LexerError {
                        start,
                        end,
                        text: self.lexer.slice(),
                    }),
                    kind => Ok((start, Token::from_kind(kind, self.lexer.slice()), end)),
                })
            }
        };
        self.lexer.advance();
        ret
    }
}

#[derive(Debug)]
pub enum Expr<'i> {
    Ident(&'i str),
    Int(i64),
}

fn main() {
    let input = "42";
    let lexer = Lexer::new(input);
    println!("{:?}", nanolang::ExprParser::new().parse(input, lexer));
}
