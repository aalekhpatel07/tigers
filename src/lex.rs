//! This is a lexer for the Tiger programming language implemented
//! using the nom crate. Various constructs implement the Parse trait
//! which is used to try parse the input into that type.

use miette::GraphicalReportHandler;
use nom::combinator::map;
use nom::multi::many0;
use nom::{IResult, branch::alt};
use nom::error::ParseError;
use nom_locate::LocatedSpan;
use nom_supreme::error::{ErrorTree, GenericErrorTree, BaseErrorKind};
use nom_supreme::final_parser::final_parser;

use crate::tokens::{
    Keyword, 
    Punctuation, 
    Identifier,
    Constant, EscapeSequence, Whitespace,
};

pub type Span<'a> = LocatedSpan<&'a str>;

pub trait Parse {
    fn parse<'a, E>(input: Span<'a>) -> IResult<Span<'a>, Self, E> where Self: Sized, E: ParseError<Span<'a>> + std::fmt::Debug;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    ReservedKeyword(Keyword),
    Identifier(Identifier),
    Punctuation(Punctuation),
    Constant(Constant),
    EscapeSequence(EscapeSequence),
    Whitespace(Whitespace),
}


impl Parse for Token {
    fn parse<'a, E>(input: Span<'a>) -> IResult<Span<'a>, Self, E> where Self: Sized, E: ParseError<Span<'a>> + std::fmt::Debug {
        alt((
            map(Keyword::parse, Token::ReservedKeyword),
            map(Identifier::parse, Token::Identifier),
            map(Punctuation::parse, Token::Punctuation),
            map(Constant::parse, Token::Constant),
            map(EscapeSequence::parse, Token::EscapeSequence),
            map(Whitespace::parse, Token::Whitespace),
        ))(input)
    }
}

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[error("bad input")]
pub struct BadInput {
    #[source_code]
    src: String,

    #[label("{kind}")]
    bad_bit: miette::SourceSpan,

    kind: BaseErrorKind<&'static str, Box<dyn std::error::Error + Send + Sync>>,
}


pub fn lex(s: &str) -> core::result::Result<Vec<Token>, BadInput> {
    let s_cloned = s.clone().to_owned();
    let input = Span::new(s);
    let maybe_tokens: Result<Vec<Token>, ErrorTree<Span>> = final_parser(many0(Token::parse::<ErrorTree<Span>>))(input);
        maybe_tokens
        .map_err(|e| {
            match e {
                GenericErrorTree::Base { location, kind } => {
                    let offset = location.location_offset().into();
                    let err = BadInput {
                        src: s_cloned,
                        bad_bit: miette::SourceSpan::new(offset, 0.into()),
                        kind,
                    };
                    let mut s = String::new();
                    GraphicalReportHandler::new()
                    .render_report(&mut s, &err)
                    .unwrap();
                    println!("{s}");
                    err
                },
                GenericErrorTree::Stack { base, contexts } => {
                    let err = BadInput {
                        src: s_cloned,
                        bad_bit: miette::SourceSpan::new(contexts.first().unwrap().0.location_offset().into(), 0.into()),
                        kind: BaseErrorKind::External(format!("Something something GenericErrorTree::Stack {:?} {:?}", base, contexts).into()),
                    };
                    println!("GenericErrorTree::Stack {:?}{:?}", base, contexts);
                    let mut s = String::new();
                    GraphicalReportHandler::new()
                    .render_report(&mut s, &err)
                    .unwrap();
                    println!("{s}");
                    err
                },
                GenericErrorTree::Alt(x) => {
                    let err = BadInput {
                        src: s_cloned,
                        bad_bit: miette::SourceSpan::new(0.into(), 0.into()),
                        kind: BaseErrorKind::External(format!("Something something GenericErrorTree::Alt {:?}", x).into()),
                    };
                    println!("GenericErrorTree::Alt {:?}", x);
                    let mut s = String::new();
                    GraphicalReportHandler::new()
                    .render_report(&mut s, &err)
                    .unwrap();
                    println!("{s}");
                    err
                }
            }
        })
}

#[cfg(test)]
mod tests {

    use super::*;
    use test_case::test_case;

    #[
        test_case(
            "let x = \"x \\\n\t\t\t \\x. \" in x + 1 end", 
            vec![
                Token::ReservedKeyword(Keyword::Let),
                Token::Identifier(Identifier("x".into())),
                Token::Whitespace(Whitespace),
                Token::Punctuation(Punctuation::Equal),
                Token::Whitespace(Whitespace),
                Token::Constant(Constant::String("x x. ".to_string())),
                Token::ReservedKeyword(Keyword::In),
                Token::Identifier(Identifier("x".into())),
                Token::Whitespace(Whitespace),
                Token::Punctuation(Punctuation::Plus),
                Token::Whitespace(Whitespace),
                Token::Constant(Constant::Integer("1".into())),
                Token::ReservedKeyword(Keyword::End),
            ]
            ; " A simple let expression is lexed correctly."
        )
    ]
    #[test_case("try(c<int)", vec![
        Token::Identifier(Identifier("try".into())),
        Token::Punctuation(Punctuation::LeftParen),
        Token::Identifier(Identifier("c".into())),
        Token::Punctuation(Punctuation::Less),
        Token::Identifier(Identifier("int".into())),
        Token::Punctuation(Punctuation::RightParen),
    ]
  ; "int is not a keyword but an identifier.")]
    #[test_case("try(c<in t)", vec![
        Token::Identifier(Identifier("try".into())),
        Token::Punctuation(Punctuation::LeftParen),
        Token::Identifier(Identifier("c".into())),
        Token::Punctuation(Punctuation::Less),
        Token::ReservedKeyword(Keyword::In),
        Token::Identifier(Identifier("t".into())),
        Token::Punctuation(Punctuation::RightParen),
    ]
  ; "int is a keyword (and consumes the space that follows) and not an identifier.")]
    fn test_lex(text: &'static str, expected: Vec<Token>) {
        let result = lex(text);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected);
    }
    
    #[test]
    #[should_panic]
    fn test_lex_broken_str() {
        lex("try(c<?in t)").unwrap();
    }

}
