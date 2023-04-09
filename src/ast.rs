use std::collections::HashMap;


#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    ConstantString(String),
    ConstantInteger(String),
    Nil,
    Neg(Box<Expr>),
    BinOp(Box<Expr>, BinOpCode, Box<Expr>),
    Error,
    LValue(Box<LValue>),
    Assign(Box<LValue>, Box<Expr>)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LValue {
    Id(String),
    FieldAccess(Box<LValue>, Box<LValue>),
    ArrayAccess(Box<LValue>, Box<Expr>)
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// In increasing order of precedence.
pub enum BinOpCode {
    Or, // | Lowest precedence.

    And, // &

    // Comparison
    LessThanEqual,
    GreaterThanEqual,
    LessThan,
    GreaterThan,
    LessThanGreaterThan,
    Equal,

    // Arithmetic
    Add,
    Sub,

    Mul, // highest-precedence.
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Array,
    Break,
    Do,
    Else,
    End,
    For,
    Function,
    If,
    In,
    Let,
    Nil,
    Of,
    Then,
    To,
    Type,
    Var,
    While
}

pub type Id = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Id(Id),
    ArrayOf(Box<Type>),
    Field(Box<HashMap<Id, Id>>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl {
    pub name: Id,
    pub r#type: Box<Type>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableDecl {
    pub name: Id,
    pub r#type: Option<Id>,
    pub body: Box<Expr>
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDecl {
    pub name: Id,
    pub arguments: Box<HashMap<Id, Id>>,
    pub return_type: Option<Id>,
    pub body: Box<Expr>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
    Type(Box<TypeDecl>),
    Variable(Box<VariableDecl>),
    Function(Box<FunctionDecl>)
}