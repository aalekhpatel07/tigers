use nom::{IResult, error::ParseError, branch::alt, combinator::map};
use nom_locate::LocatedSpan;
use crate::*;


// pub type Span<'a, T> = LocatedSpan<&'a T>;

// pub trait Parse<T> where T: Lex + std::fmt::Debug {
//     fn parse<'a, E>(input: Span<'a, T>) -> IResult<Span<'a, T>, Self, E> where Self: Sized, E: ParseError<Span<'a, T>> + std::fmt::Debug;
// }

// impl<T> Parse<T> for LValue
// where T: Lex + std::fmt::Debug
// {
//     fn parse<'a, E>(input: Span<'a, T>) -> IResult<Span<'a, T>, Self, E> where Self: Sized, E: ParseError<Span<'a, T>> + std::fmt::Debug {
//         println!("input: {:?}", input);
//         panic!("at the disco");
//         // let (rem, identifier) = Identifier::parse(input)?;
//         // Ok((rem, LValue::Identifier(identifier)))
//     }
// }

pub type TokenSlice<'s> = &'s [Token];

pub trait Parse {
    fn parse<'s, E>(
        tokens: TokenSlice<'s>
    ) -> IResult<TokenSlice<'s>, Self, E> 
        where 
            Self: Sized, E: ParseError<TokenSlice<'s>> + std::fmt::Debug;
}


impl Parse for Identifier {
    fn parse<'t, 's, E>(tokens: TokenSlice<'s>) -> IResult<TokenSlice<'s>, Self, E> where Self: Sized, E: ParseError<TokenSlice<'s>> + std::fmt::Debug {

        tokens
        .split_first()
        .map(|(token, rem)| {
            if let Token::Identifier(identifier) = token.clone() {
                (rem, identifier.clone())
            } else {
                // (tokens, Identifier::new("".to_string()))
                panic!("Expected identifier, got {:?}", token);       
            }
        })
        .ok_or(nom::Err::Error(E::from_error_kind(tokens, nom::error::ErrorKind::Eof)))
    }
}

impl Parse for LValue {
    fn parse<'s, E>(tokens: TokenSlice<'s>) -> IResult<TokenSlice<'s>, Self, E> where Self: Sized, E: ParseError<TokenSlice<'s>> + std::fmt::Debug {
        alt((
            map(Identifier::parse, LValue::Identifier),
        ))(tokens)
    }
}

#[cfg(test)]
mod tests {
    use nom_supreme::error::ErrorTree;

    use super::*;
    use crate::Lex;

    #[test]
    fn test_parse() {
        let input = Span::new("hello");
            // match lex(input) {
            //     Ok(tokens) => {
        let tokens = lex(&input).unwrap();
        let token_slices = tokens.as_slice();
        let lvalue = LValue::parse::<'_, ErrorTree<TokenSlice<'_>>>(token_slices).unwrap();
        println!("lvalue: {:?}", lvalue);
        // let result = Punctuation::lex::<ErrorTree<Span<'static>>>(input);
        // let result = LValue::parse(&tokens);
        // println!("result: {:?}", result);
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LValue {
    Identifier(Identifier),
    ArrayAccess(Box<LValue>, Box<Expression>),
    RecordAccess(Box<LValue>, Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    /// String constant or Integer constant.
    Constant(Constant),
    /// A None value that can be inserted in any Record or Array that expects a concrete typed value.
    Nil,
    LValue(Box<LValue>),
    Negation(Box<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    Assignment(Box<LValue>, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    Sequence(Vec<Expression>),
    TypeDefinition(TypeId, Vec<Expression>),
    ArrayOfTypeDefinition(TypeId, Box<Expression>),
    IfThen(Box<Expression>, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Expression>, Box<Expression>),
    WhileDo(Box<Expression>, Box<Expression>),
    ForToDo(Identifier, Box<Expression>, Box<Expression>, Box<Expression>),
    Break(Keyword),
    LetInEnd(Vec<Declaration>, Vec<Expression>),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    TypeId(TypeId),
    /// { type-fields opt }
    TypeFields(Vec<(Identifier, TypeId)>),
    /// array of `type-id`
    ArrayOfType(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDeclaration {
    // type `type-id` = `type`
    Type(TypeId, Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableDeclaration {
    // var `identifier` : `type-id` = `expression`
    VariableTyped(Identifier, TypeId, Expression),
    // var `identifier` = `expression`
    VariableUnTyped(Identifier, Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionDeclaration {
    /// function `id` ( type-fields opt) = `expr`
    Procedure(Identifier, Type, Expression),
    /// function `id` ( type-fields opt) : `type-id` = `expr`
    Function(Identifier, Type, TypeId, Expression)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
    TypeDeclaration(TypeDeclaration),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Times,
    Divide,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
}