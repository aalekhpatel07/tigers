
use nom::character::complete::one_of;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use nom::error::ParseError;
use tracing::error;
use std::fmt::Display;


use lazy_static::lazy_static;

use crate::lex::{Parse, Span};

lazy_static! {
    static ref LETTERS: String = String::from("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
    static ref DIGITS: String = String::from("0123456789");
    static ref UNDERSCORE: String = String::from("_");
    static ref LETTERS_OR_DIGITS_OR_UNDERSCORE: String = String::from("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_");
    static ref VALID_CONTROL_CHARS: String = String::from(r#"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_"#);
}




#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier(pub(crate) String);

pub type TypeId = Identifier;

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Parse for Identifier {

    fn parse<'a, E: ParseError<Span<'a>> + std::fmt::Debug >(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
        map(
            tuple((
                Letter::parse::<E>,
                many0(
                    one_of(LETTERS_OR_DIGITS_OR_UNDERSCORE.as_str())
                ),
            )),
            |(first, rest)| Identifier(format!("{}{}", first, rest.iter().collect::<String>())),
        )(input)
    }
}


pub use small::*;

mod small {
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::character::complete::{one_of, multispace1, digit1, space1, anychar, newline};
    use nom::combinator::{map, all_consuming, opt, map_res};
    use nom::IResult;
    use nom::error::{ParseError};
    use nom::multi::{separated_list0};
    use nom::sequence::{preceded, tuple, delimited, terminated};
    use std::fmt::Display;
    use crate::lex::{Parse, Span};
    use super::{LETTERS, DIGITS, VALID_CONTROL_CHARS};
    

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Letter(char);

    impl Parse for Letter {
        fn parse<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
            map(
                one_of(LETTERS.as_str()),
                |c| Letter(c),
            )(input)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Digit(pub(crate) char);

    impl Parse for Digit {
        fn parse<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
            map(
                one_of(DIGITS.as_str()),
                |c| Digit(c),
            )(input)
        }
    }

    impl Display for Letter {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Whitespace;

    impl Parse for Whitespace {
        fn parse<'a, E>(input: Span<'a>) -> IResult<Span<'a>, Self, E> where E: ParseError<Span<'a>> + std::fmt::Debug {
            map(
                multispace1,
                |_| Whitespace,
            )(input)
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Punctuation {
        LeftParen,
        RightParen,
        LeftBrace,
        RightBrace,
        LeftBracket,
        RightBracket,
        Semicolon,
        Colon,
        Comma,
        Dot,
        Plus,
        Minus,
        Star,
        Slash,
        Equal,
        Fish,
        Greater,
        GreaterEqual,
        Less,
        LessEqual,
        Ampersand,
        Pipe,
        ColonEqual,
        CommentStart,
        CommentEnd,
    }

    fn is_not_double_quote(c: char) -> bool {
        c != '"'
    }
    fn is_not_backslash(c: char) -> bool {
        c != '\\'
    }

    impl Parse for Punctuation {
        fn parse<'a, E>(input: Span<'a>) -> IResult<Span<'a>, Self, E> 
            where E: ParseError<Span<'a>>
        {
            map(
                alt((
                    alt((
                        tag("/*"),
                        tag("*/"),
                        tag(">="),
                        tag("<="),
                        tag("<>"),
                        tag(":="),
                        tag("&"),
                        tag("|"),
                        tag("<"),
                        tag(">"),
                    )),
                    tag("("),
                    tag(")"),
                    tag("{"),
                    tag("}"),
                    tag("["),
                    tag("]"),
                    tag(";"),
                    tag(":"),
                    tag(","),
                    tag("."),
                    tag("+"),
                    tag("-"),
                    tag("*"),
                    tag("/"),
                    tag("="),
                )), 
                |v: Span<'a>| {
                    match v.as_ref() {
                        "(" => Punctuation::LeftParen,
                        ")" => Punctuation::RightParen,
                        "{" => Punctuation::LeftBrace,
                        "}" => Punctuation::RightBrace,
                        "[" => Punctuation::LeftBracket,
                        "]" => Punctuation::RightBracket,
                        ";" => Punctuation::Semicolon,
                        ":" => Punctuation::Colon,
                        "," => Punctuation::Comma,
                        "." => Punctuation::Dot,
                        "+" => Punctuation::Plus,
                        "-" => Punctuation::Minus,
                        "*" => Punctuation::Star,
                        "/" => Punctuation::Slash,
                        "=" => Punctuation::Equal,
                        "<>" => Punctuation::Fish,
                        ">" => Punctuation::Greater,
                        ">=" => Punctuation::GreaterEqual,
                        "<" => Punctuation::Less,
                        "<=" => Punctuation::LessEqual,
                        "&" => Punctuation::Ampersand,
                        "|" => Punctuation::Pipe,
                        ":=" => Punctuation::ColonEqual,
                        "/*" => Punctuation::CommentStart,
                        "*/" => Punctuation::CommentEnd,
                        c => {
                            unreachable!("Unrecognized punctuation: {}", c)
                        }
                    }
                }
            )(input)
        }
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

    impl Parse for Keyword {
        fn parse<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {


            let tag_parser_factory = || {
                alt((
                    tag("array"),
                    tag("break"),
                    tag("do"),
                    tag("else"),
                    tag("end"),
                    tag("for"),
                    tag("function"),
                    tag("if"),
                    tag("in"),
                    tag("let"),
                    tag("nil"),
                    tag("of"),
                    tag("then"),
                    tag("to"),
                    tag("type"),
                    tag("var"),
                    tag("while"),
                ))
            };

            let tag_parser0 = tag_parser_factory();
            // let tag_parser1 = tag_parser_factory();
            // let tag_parser2 = tag_parser_factory();
            // let tag_parser3 = tag_parser_factory();

            // Currently, keywords consume the surrounding whitespace. Do we need to?
            // Idk I'll leave it for now.
            
            // FIXME: This is broken in the sense that keyword detection is not robust.
            // What we really want is to have spaces on either side of the keyword,
            // or if the keyword is at the beginning or end of the input, then we
            // want to have a space on the side of the keyword that is not at the
            // beginning or end of the input. I'm not too sure how to go about that
            // but its also 6:30am so screw it.
            map(
            alt((
                        delimited(
                            space1,
                            tag_parser0,
                            space1
                        ),
                )),
                // alt((
                //     // Space-separated.
                //     // delimited(
                //     //     space1,
                //     //     tag_parser0,
                //     //     space1
                //     // ),
                //     // delimited(
                //     //     newline,
                //     //     tag_parser1,
                //     //     space1
                //     // ),
                //     // delimited(
                //     //     space1,
                //     //     tag_parser2,
                //     //     newline
                //     // ),
                    
                //     all_consuming(
                //         tag_parser3
                //     )
                // )),
                |v: Span<'a>| {
                    match v.as_ref() {
                        "array" => Keyword::Array,
                        "break" => Keyword::Break,
                        "do" => Keyword::Do,
                        "else" => Keyword::Else,
                        "end" => Keyword::End,
                        "for" => Keyword::For,
                        "function" => Keyword::Function,
                        "if" => Keyword::If,
                        "in" => Keyword::In,
                        "let" => Keyword::Let,
                        "nil" => Keyword::Nil,
                        "of" => Keyword::Of,
                        "then" => Keyword::Then,
                        "to" => Keyword::To,
                        "type" => Keyword::Type,
                        "var" => Keyword::Var,
                        "while" => Keyword::While,
                        c => {
                            unreachable!("Unrecognized keyword: {}", c)
                        }
                    }
                }
            )(input)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Constant {
        Integer(String),
        String(String)
    }

    impl Parse for Constant {
        fn parse<'a, E: ParseError<Span<'a>> + std::fmt::Debug>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
            alt((
                map(
                    digit1,
                    |v: Span<'a>| {
                        Constant::Integer(v.to_string())
                    }
                ),

                map(
                    delimited(
                        nom::character::complete::char('"'), 
                        take_while(is_not_double_quote),
                        nom::character::complete::char('"'), 
                    ),
                    |text: Span<'a>| {

                        let strings: Result<(Span<'a>, Vec<Span<'a>>), _> =
                            separated_list0(
                            tuple((
                                    tag("\\"),
                                    multispace1::<_, E>,
                                    tag("\\")
                                )),
                                take_while(is_not_backslash)
                            )(text);

                        strings.map(|(_, individual_parts)| {
                            let mut s = String::new();
                            for part in individual_parts {
                                s.push_str(&part);
                            }
                            Constant::String(s)
                        }).expect("Failed to lex string literal.")
                    }
                )
            ))(input)
        }
    }


    // Multiline strings are a bit finnicky. 
    // Might have to break into a more fine grained lexing pattern later.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum EscapeSequence {
        Newline,
        Tab,
        DoubleQuote,
        Backslash,
        Control(char),
        ASCIICode([Digit; 3]),
    }

    impl Parse for EscapeSequence {
        fn parse<'a, E: ParseError<Span<'a>> + std::fmt::Debug>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
            preceded(
            tag(r#"\"#),
            alt((
                        map(
                    alt((
                                tag("n"),
                                tag("t"),
                                tag(r#"""#),
                                tag(r#"\"#),
                            )),
                            |v: Span<'a>| {
                                match v.as_ref() {
                                    "n" => EscapeSequence::Newline,
                                    "t" => EscapeSequence::Tab,
                                    "\"" => EscapeSequence::DoubleQuote,
                                    "\\" => EscapeSequence::Backslash,
                                    c => {
                                        unreachable!("Unrecognized escape sequence: {}", c)
                                    }
                                }
                            }
                        ),
                        map(
                            preceded(
                                tag("^"),
                                one_of(VALID_CONTROL_CHARS.as_str()),
                            ),
                            |c| EscapeSequence::Control(c),
                        ),
                        map(
                            preceded(
                                tag("x"),
                                tuple((
                                    Digit::parse::<E>,
                                    Digit::parse::<E>,
                                    Digit::parse::<E>,
                                )),
                            ),
                            |(d1, d2, d3)| EscapeSequence::ASCIICode([d1, d2, d3]),
                        ),
                    )),
                )(input)
            }
    }

}

// TODO: Implement the parsing for expressions.
pub use expr::*;
mod expr {
    use super::*;

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

}


#[cfg(test)]
mod tests {
    use nom_supreme::error::ErrorTree;
    use super::*;
    use test_case::test_case;

    #[test_case("he!llo1", "he"; "Incomplete identifier is recognized")]
    #[test_case("hello", "hello"; "Identifier is recognized")]
    #[test_case("hello_world", "hello_world"; "Identifier with underscore is recognized")]
    #[test_case("hello123_123", "hello123_123"; "Identifier with numbers and underscore is recognized")]
    fn test_identifier(text: &'static str, identifier: &'static str) {
        let input = Span::new(text);
        let result = Identifier::parse::<ErrorTree<Span<'static>>>(input);
        assert_eq!(result.unwrap().1, Identifier(String::from(identifier)));
    }


    #[test_case("&", Punctuation::Ampersand ; "Ampersand is recognized")]
    #[test_case("|", Punctuation::Pipe ; "Pipe is recognized")]
    #[test_case("(", Punctuation::LeftParen ; "LeftParen is recognized")]
    #[test_case(")", Punctuation::RightParen ; "RightParen is recognized")]
    #[test_case("{", Punctuation::LeftBrace ; "LeftBrace is recognized")]   
    #[test_case("}", Punctuation::RightBrace ; "RightBrace is recognized")]
    #[test_case("[", Punctuation::LeftBracket ; "LeftBracket is recognized")]
    #[test_case("]", Punctuation::RightBracket ; "RightBracket is recognized")]
    #[test_case(";", Punctuation::Semicolon ; "Semicolon is recognized")]
    #[test_case(":", Punctuation::Colon ; "Colon is recognized")]
    #[test_case(",", Punctuation::Comma ; "Comma is recognized")]
    #[test_case(".", Punctuation::Dot ; "Dot is recognized")]
    #[test_case("+", Punctuation::Plus ; "Plus is recognized")]
    #[test_case("-", Punctuation::Minus ; "Minus is recognized")]
    #[test_case("*", Punctuation::Star ; "Star is recognized")]
    #[test_case("/", Punctuation::Slash ; "Slash is recognized")]
    #[test_case("=", Punctuation::Equal ; "Equal is recognized")]
    #[test_case("<>", Punctuation::Fish ; "Fish is recognized")]
    #[test_case(">", Punctuation::Greater ; "Greater is recognized")]
    #[test_case(">=", Punctuation::GreaterEqual ; "GreaterEqual is recognized")]
    #[test_case("<", Punctuation::Less ; "Less is recognized")]
    #[test_case("<=", Punctuation::LessEqual ; "LessEqual is recognized")]
    #[test_case(":=", Punctuation::ColonEqual ; "ColonEqual is recognized")]
    #[test_case("/*", Punctuation::CommentStart ; "CommentStart is recognized")]
    #[test_case("*/", Punctuation::CommentEnd ; "CommentEnd is recognized")]
    fn test_punctuation(text: &'static str, expected: Punctuation) {
        let input = Span::new(text);
        let result = Punctuation::parse::<ErrorTree<Span<'static>>>(input);
        assert_eq!(result.unwrap().1, expected);
    }

    #[test_case(r#"\n"#, EscapeSequence::Newline ; "Newline is recognized")]
    #[test_case(r#"\t"#, EscapeSequence::Tab ; "Tab is recognized")]
    #[test_case(r#"\\"#, EscapeSequence::Backslash ; "Backslash is recognized")]
    #[test_case(r#"\""#, EscapeSequence::DoubleQuote ; "Double quote is recognized")]
    #[test_case(r#"\x093"#, EscapeSequence::ASCIICode([Digit('0'), Digit('9'), Digit('3')]) ; "ASCIICode is recognized")]
    #[test_case(r#"\^C"#, EscapeSequence::Control('C') ; "Control C is recognized")]
    #[test_case(r#"\^["#, EscapeSequence::Control('[') ; "Control bracket open is recognized")]
    #[test_case(r#"\^]"#, EscapeSequence::Control(']') ; "Control bracket close is recognized")]
    #[test_case(r#"\^^"#, EscapeSequence::Control('^') ; "Control caret is recognized")]
    #[test_case(r#"\^_"#, EscapeSequence::Control('_') ; "Control underscore is recognized")]
    fn test_escape_sequence(text: &'static str, expected: EscapeSequence) {
        let input = Span::new(text);
        let result = EscapeSequence::parse::<ErrorTree<Span<'static>>>(input);
        assert_eq!(result.unwrap().1, expected);
    }

    #[test_case("array", Keyword::Array ; "Array is recognized")]
    #[test_case("break", Keyword::Break ; "Break is recognized")]
    #[test_case("do", Keyword::Do ; "Do is recognized")]
    #[test_case("else", Keyword::Else ; "Else is recognized")]
    #[test_case("end", Keyword::End ; "End is recognized")]
    #[test_case("for", Keyword::For ; "For is recognized")]
    #[test_case("function", Keyword::Function ; "Function is recognized")]
    #[test_case("if", Keyword::If ; "If is recognized")]
    #[test_case("in", Keyword::In ; "In is recognized")]
    #[test_case("let", Keyword::Let ; "Let is recognized")]
    #[test_case("nil", Keyword::Nil ; "Nil is recognized")]
    #[test_case("of", Keyword::Of ; "Of is recognized")]
    #[test_case("then", Keyword::Then ; "Then is recognized")]
    #[test_case("to", Keyword::To ; "To is recognized")]
    #[test_case("type", Keyword::Type ; "Type is recognized")]
    #[test_case("var", Keyword::Var ; "Var is recognized")]
    #[test_case("while", Keyword::While ; "While is recognized")]
    fn test_keyword(text: &'static str, expected: Keyword) {
        let input = Span::new(text);
        let result = Keyword::parse::<ErrorTree<Span<'static>>>(input);
        assert_eq!(result.unwrap().1, expected);
    }

    #[test_case("012345", Constant::Integer("012345".into()); "Integer is recognized")]
    #[test_case(r#""hello""#, Constant::String("hello".into()); "String is recognized.")]
    #[test_case("\"hello \\\n\t\\world\"", Constant::String("hello world".into()); "Multiline string is recognized.")]
    #[test_case("\"hello \\\n\t\\world.\\\n\t\t\t\t\n\n\n\t\\ stuff\"", Constant::String("hello world. stuff".into()); "Multiline long string is recognized.")]
    fn test_constant(text: &'static str, expected: Constant) {
        let input = Span::new(text);
        let result = Constant::parse::<ErrorTree<Span<'static>>>(input);
        assert_eq!(result.unwrap().1, expected);
    }

}