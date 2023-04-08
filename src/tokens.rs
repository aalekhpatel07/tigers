
use nom::character::complete::one_of;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use nom::error::ParseError;
use std::fmt::Display;


use lazy_static::lazy_static;

use crate::lex::{Lex, Span};

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

impl Lex for Identifier {
    fn lex<'a, E: ParseError<Span<'a>> + std::fmt::Debug >(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
        map(
            tuple((
                Letter::lex::<E>,
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
    use nom::character::complete::{one_of, multispace1, digit1, space0};
    use nom::combinator::map;
    use nom::IResult;
    use nom::error::ParseError;
    use nom::multi::separated_list0;
    use nom::sequence::{preceded, tuple, delimited};
    use std::fmt::Display;
    use crate::Identifier;
    use crate::lex::{Lex, Span};
    use super::{LETTERS, DIGITS, VALID_CONTROL_CHARS};
    

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Letter(char);

    impl Lex for Letter {
        fn lex<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
            map(
                one_of(LETTERS.as_str()),
                |c| Letter(c),
            )(input)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Digit(pub(crate) char);

    impl Lex for Digit {
        fn lex<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
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

    impl Lex for Whitespace {
        fn lex<'a, E>(input: Span<'a>) -> IResult<Span<'a>, Self, E> where E: ParseError<Span<'a>> + std::fmt::Debug {
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

    impl Lex for Punctuation {
        fn lex<'a, E>(input: Span<'a>) -> IResult<Span<'a>, Self, E> 
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

    impl TryFrom<Identifier> for Keyword {
        type Error = String;
        fn try_from(value: Identifier) -> Result<Self, Self::Error> {
            match value.0.as_str() {
                "array" => Ok(Keyword::Array),
                "break" => Ok(Keyword::Break),
                "do" => Ok(Keyword::Do),
                "else" => Ok(Keyword::Else),
                "end" => Ok(Keyword::End),
                "for" => Ok(Keyword::For),
                "function" => Ok(Keyword::Function),
                "if" => Ok(Keyword::If),
                "in" => Ok(Keyword::In),
                "let" => Ok(Keyword::Let),
                "nil" => Ok(Keyword::Nil),
                "of" => Ok(Keyword::Of),
                "then" => Ok(Keyword::Then),
                "to" => Ok(Keyword::To),
                "type" => Ok(Keyword::Type),
                "var" => Ok(Keyword::Var),
                "while" => Ok(Keyword::While),
                _ => Err(format!("{} is not a keyword", value.0)),
            }
        }
    }

    impl Lex for Keyword {
        fn lex<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {


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
            
            // Fixed by treating keywords as identifiers in the first pass.
            // Then, we can check if they are keywords in the second pass.
            // This is a bit of a hack, but it works.
            map(
                delimited(
                    space0,
                    tag_parser0,
                    space0
                ),
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

    impl Lex for Constant {
        fn lex<'a, E: ParseError<Span<'a>> + std::fmt::Debug>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
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
                                take_while(|_| true)
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

    impl Lex for EscapeSequence {
        fn lex<'a, E: ParseError<Span<'a>> + std::fmt::Debug>(input: Span<'a>) -> IResult<Span<'a>, Self, E> {
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
                                    Digit::lex::<E>,
                                    Digit::lex::<E>,
                                    Digit::lex::<E>,
                                )),
                            ),
                            |(d1, d2, d3)| EscapeSequence::ASCIICode([d1, d2, d3]),
                        ),
                    )),
                )(input)
            }
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
        let result = Identifier::lex::<ErrorTree<Span<'static>>>(input);
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
        let result = Punctuation::lex::<ErrorTree<Span<'static>>>(input);
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
        let result = EscapeSequence::lex::<ErrorTree<Span<'static>>>(input);
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
        let result = Keyword::lex::<ErrorTree<Span<'static>>>(input);
        assert_eq!(result.unwrap().1, expected);
    }

    #[test_case("012345", Constant::Integer("012345".into()); "Integer is recognized")]
    #[test_case(r#""hello""#, Constant::String("hello".into()); "String is recognized.")]
    #[test_case(r#""\n""#, Constant::String("\\n".into()); "Newline in a string is recognized.")]
    #[test_case("\"hello \\\n\t\\world\"", Constant::String("hello \\\n\t\\world".into()); "Multiline string is recognized.")]
    #[test_case("\"hello \\\n\t\\world.\\\n\t\t\t\t\n\n\n\t\\ stuff\"", Constant::String("hello \\\n\t\\world.\\\n\t\t\t\t\n\n\n\t\\ stuff".into()); "Multiline long string is recognized.")]
    fn test_constant(text: &'static str, expected: Constant) {
        let input = Span::new(text);
        let result = Constant::lex::<ErrorTree<Span<'static>>>(input);
        assert_eq!(result.unwrap().1, expected);
    }

}