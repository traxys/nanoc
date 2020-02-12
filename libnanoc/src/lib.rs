#![allow(unused_parens)]
use logos::Logos;
use std::collections::HashMap;

#[macro_use]
extern crate lalrpop_util;

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
    #[token = "fn"]
    Function,
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
    Function,
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
            TokenKind::Function => Self::Function,
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
pub enum Param<'i> {
    Expr(UndecoratedTree<'i>),
    Placeholder,
}

pub fn desugar_call<'i>(func: UndecoratedTree<'i>, args: Vec<Param<'i>>) -> UndecoratedTree<'i> {
    let mut new_args = Vec::new();
    let mut params = Vec::new();
    let mut current = 0;
    for arg in args {
        match arg {
            Param::Expr(e) => new_args.push(e),
            Param::Placeholder => {
                params.push(Binding::Unamed(current));
                new_args.push(UndecoratedTree::from_node(UndecoratedNode::Unamed(current)));
                current += 1;
            }
        }
    }
    let call = UndecoratedTree::from_node(UndecoratedNode::Call {
        func: Box::new(func),
        args: new_args,
    });
    if params.is_empty() {
        call
    } else {
        UndecoratedTree::from_node(UndecoratedNode::FuncDefinition {
            name: None,
            bindings: params,
            body: Box::new(call),
        })
    }
}

#[derive(Debug)]
pub enum Binding<'i> {
    Named(&'i str),
    Unamed(isize),
}

pub type UndecoratedTree<'i> = DecoratedTree<'i, ()>;
pub type UndecoratedNode<'i> = DecoratedNode<'i, ()>;

#[derive(Debug)]
pub struct DecoratedTree<'i, T> {
    decoration: T,
    pub node: DecoratedNode<'i, T>,
}

impl<'i, T> DecoratedTree<'i, T> {
    pub fn map_decoration<U>(self, f: &impl Fn(T) -> U) -> DecoratedTree<'i, U> {
        DecoratedTree {
            decoration: f(self.decoration),
            node: self.node.map_decoration(f),
        }
    }
}

impl<'i> UndecoratedTree<'i> {
    pub fn from_node(node: UndecoratedNode<'i>) -> Self {
        UndecoratedTree {
            decoration: (),
            node,
        }
    }
}

#[derive(Debug)]
pub enum DecoratedNode<'i, T> {
    Unamed(isize),
    Ident(&'i str),
    Int(i64),
    Call {
        func: Box<DecoratedTree<'i, T>>,
        args: Vec<DecoratedTree<'i, T>>,
    },
    FuncDefinition {
        name: Option<&'i str>,
        bindings: Vec<Binding<'i>>,
        body: Box<DecoratedTree<'i, T>>,
    },
}

impl<'i, T> DecoratedNode<'i, T> {
    fn map_decoration<U>(self, f: &impl Fn(T) -> U) -> DecoratedNode<'i, U> {
        match self {
            DecoratedNode::Unamed(i) => DecoratedNode::Unamed(i),
            DecoratedNode::Ident(n) => DecoratedNode::Ident(n),
            DecoratedNode::Int(v) => DecoratedNode::Int(v),
            DecoratedNode::Call { func, args } => DecoratedNode::Call {
                func: Box::new(func.map_decoration(f)),
                args: args
                    .into_iter()
                    .map(|node| node.map_decoration(f))
                    .collect(),
            },
            DecoratedNode::FuncDefinition {
                name,
                bindings,
                body,
            } => DecoratedNode::FuncDefinition {
                name,
                bindings,
                body: Box::new(body.map_decoration(f)),
            },
        }
    }
}

enum Type {
    Int,
    Function {
        inputs: Vec<Type>,
        output: Box<Type>,
    },
}

struct Definition {
    ty: Type,
}

struct Scope<'i, 'p> {
    parent: Option<&'p Scope<'i, 'p>>,
    scope: HashMap<Binding<'i>, Definition>,
}

pub fn parse(
    input: &str,
) -> Result<UndecoratedTree<'_>, lalrpop_util::ParseError<usize, Token<'_>, LexerError<'_>>> {
    let lexer = Lexer::new(input);
    nanolang::ExprParser::new().parse(input, lexer)
}
