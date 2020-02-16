#![allow(unused_parens)]
use logos::Logos;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    #[token = "()"]
    Unit,
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
    #[token = "let"]
    Let,
    #[token = ":"]
    Colon,
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
    Let,
    Colon,
    Unit,
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
            TokenKind::Let => Self::Let,
            TokenKind::Int => Self::Int(input.parse().expect("int is not an int")),
            TokenKind::Colon => Self::Colon,
            TokenKind::Unit => Self::Unit,
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
                params.push(Binding {
                    kind: BindingKind::Unamed(current),
                    ty: None,
                });
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

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum BindingKind<'i> {
    Named(&'i str),
    Unamed(isize),
}

#[derive(Debug)]
pub struct Binding<'i> {
    kind: BindingKind<'i>,
    ty: Option<Type>,
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
    Bind {
        name: Binding<'i>,
        value: Box<DecoratedTree<'i, T>>,
    },
    List {
        statements: Vec<DecoratedTree<'i, T>>,
        expr: Box<DecoratedTree<'i, T>>,
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
            DecoratedNode::Bind { name, value } => DecoratedNode::Bind {
                name,
                value: Box::new(value.map_decoration(f)),
            },
            DecoratedNode::List { statements, expr } => DecoratedNode::List {
                statements: statements
                    .into_iter()
                    .map(|node| node.map_decoration(f))
                    .collect(),
                expr: Box::new(expr.map_decoration(f)),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Int,
    Function {
        inputs: Vec<Type>,
        output: Box<Type>,
    },
}

#[derive(Debug, Clone)]
struct Definition {
    ty: Type,
}

#[derive(Debug)]
struct Scope<'i> {
    parent: Option<SharedScope<'i>>,
    bindings: HashMap<BindingKind<'i>, Definition>,
}
impl<'i> Scope<'i> {
    pub fn base() -> Self {
        let mut bindings = HashMap::new();
        bindings.insert(
            BindingKind::Named("print_int"),
            Definition {
                ty: Type::Function {
                    inputs: vec![Type::Int],
                    output: Box::new(Type::Unit),
                },
            },
        );
        Self {
            bindings,
            parent: None,
        }
    }
    fn register(&mut self, binding: &Binding<'i>) {
        self.bindings.insert(
            binding.kind,
            Definition {
                ty: binding
                    .ty
                    .clone()
                    .expect("binding had no type assertion, type inference in unimplemented"),
            },
        );
    }
    fn get_ident(&self, ident: &'i str) -> Option<Definition> {
        match self.bindings.get(&BindingKind::Named(ident)) {
            some @ Some(_) => some.cloned(),
            None => self
                .parent
                .as_ref()
                .map(|parent| parent.borrow().get_ident(ident))
                .flatten(),
        }
    }
    fn from_parent(parent: SharedScope<'i>) -> SharedScope<'i> {
        Rc::new(RefCell::new(Scope {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }
}

type SharedScope<'i> = Rc<RefCell<Scope<'i>>>;

#[derive(Debug)]
pub struct ScopeTypeDecoration<'i> {
    scope: SharedScope<'i>,
    ty: Type,
}

pub type ScopedTypedTree<'i> = DecoratedTree<'i, ScopeTypeDecoration<'i>>;
pub type ScopedTypedNode<'i> = DecoratedNode<'i, ScopeTypeDecoration<'i>>;

pub fn type_tree(tree: UndecoratedTree<'_>) -> Result<ScopedTypedTree<'_>, TypeError<'_>> {
    let base = Rc::new(RefCell::new(Scope::base()));
    scope(tree, base)
}

#[derive(Debug)]
pub struct TypeError<'i> {
    offending_tree: Option<ScopedTypedTree<'i>>,
    kind: TypeErrorKind<'i>,
}
#[derive(Debug)]
pub enum TypeErrorKind<'i> {
    InvalidType { expected: Type, got: Type },
    NoSuchIdent { binding: &'i str },
    ExpectedFunctionType { got: Type },
}

fn scope<'i>(
    tree: UndecoratedTree<'i>,
    parent: SharedScope<'i>,
) -> Result<ScopedTypedTree<'i>, TypeError<'i>> {
    match tree.node {
        UndecoratedNode::List { statements, expr } => {
            let list_scope = Scope::from_parent(parent.clone());
            let statements: Result<Vec<_>, _> = statements
                .into_iter()
                .map(|node| -> Result<_, TypeError<'i>> {
                    let scoped = scope(node, list_scope.clone())?;
                    add_to_scope(&scoped.node, &list_scope);
                    Ok(scoped)
                })
                .collect();
            let expr = Box::new(scope(*expr, list_scope.clone())?);
            Ok(ScopedTypedTree {
                decoration: ScopeTypeDecoration {
                    scope: parent,
                    ty: expr.decoration.ty.clone(),
                },
                node: ScopedTypedNode::List {
                    statements: statements?,
                    expr,
                },
            })
        }
        UndecoratedNode::Bind { name, value } => {
            let value = Box::new(scope(*value, parent.clone())?);
            let expected = name.ty.as_ref().expect("type inference is todo");
            let got = &value.decoration.ty;
            if expected != got {
                return Err(TypeError {
                    kind: TypeErrorKind::InvalidType {
                        expected: expected.clone(),
                        got: got.clone(),
                    },
                    offending_tree: Some(*value),
                });
            }
            Ok(ScopedTypedTree {
                decoration: ScopeTypeDecoration {
                    scope: parent,
                    ty: value.decoration.ty.clone(),
                },
                node: ScopedTypedNode::Bind { name, value },
            })
        }
        UndecoratedNode::Unamed(_) => todo!("unamed"),
        UndecoratedNode::Ident(n) => {
            let def = match parent.borrow().get_ident(n) {
                Some(d) => d,
                None => {
                    return Err(TypeError {
                        offending_tree: None,
                        kind: TypeErrorKind::NoSuchIdent { binding: n },
                    })
                }
            };
            Ok(ScopedTypedTree {
                decoration: ScopeTypeDecoration {
                    ty: def.ty.clone(),
                    scope: parent,
                },
                node: ScopedTypedNode::Ident(n),
            })
        }
        UndecoratedNode::Int(v) => Ok(ScopedTypedTree {
            decoration: ScopeTypeDecoration {
                scope: parent,
                ty: Type::Int,
            },
            node: ScopedTypedNode::Int(v),
        }),
        UndecoratedNode::FuncDefinition {
            bindings,
            body,
            name,
        } => {
            let function_scope = Scope::from_parent(parent.clone());
            let inputs: Vec<_> = bindings
                .iter()
                .map(|binding| {
                    function_scope.borrow_mut().register(&binding);
                    binding.ty.clone().expect("no type inference yet")
                })
                .collect();
            // TODO: add a self type for the function definition.
            // I think type inference will help
            let body = Box::new(scope(*body, function_scope)?);
            let function_type = Type::Function {
                inputs,
                output: Box::new(body.decoration.ty.clone()),
            };
            Ok(ScopedTypedTree {
                decoration: ScopeTypeDecoration {
                    ty: function_type,
                    scope: parent,
                },
                node: ScopedTypedNode::FuncDefinition {
                    name,
                    bindings,
                    body,
                },
            })
        }
        UndecoratedNode::Call { func, args } => {
            let func = Box::new(scope(*func, parent.clone())?);
            let (inputs, output) = match &func.decoration.ty {
                Type::Function { inputs, output } => (inputs, output),
                t => {
                    return Err(TypeError {
                        kind: TypeErrorKind::ExpectedFunctionType { got: t.clone() },
                        offending_tree: Some(*func),
                    })
                }
            };
            let args: Result<_, _> = args
                .into_iter()
                .zip(inputs)
                .map(|(arg, ty)| -> Result<_, TypeError> {
                    let arg = scope(arg, parent.clone())?;
                    if *ty != arg.decoration.ty {
                        return Err(TypeError {
                            kind: TypeErrorKind::InvalidType {
                                expected: ty.clone(),
                                got: arg.decoration.ty.clone(),
                            },
                            offending_tree: Some(arg),
                        });
                    }
                    Ok(arg)
                })
                .collect();
            Ok(ScopedTypedTree {
                decoration: ScopeTypeDecoration {
                    scope: parent,
                    ty: *output.clone(),
                },
                node: ScopedTypedNode::Call { func, args: args? },
            })
        }
    }
}
fn add_to_scope<'i, 'p, T>(statement: &DecoratedNode<'i, T>, scope: &SharedScope<'i>) {
    if let DecoratedNode::Bind { name, .. } = statement {
        scope.borrow_mut().register(name)
    }
}

pub fn parse(
    input: &str,
) -> Result<UndecoratedTree<'_>, lalrpop_util::ParseError<usize, Token<'_>, LexerError<'_>>> {
    let lexer = Lexer::new(input);
    nanolang::StatementParser::new().parse(input, lexer)
}
