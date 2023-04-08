use tigers::tiger::TermParser;
use test_case::test_case;

#[test_case("22"; "Simple number without any parenthesis is a valid term.")]
#[test_case("(22)"; "Simple number with one pair of parenthesis is a valid term.")]
#[test_case("(((22)))"; "Simple number with a few pairs of parenthesis is a valid term.")]
fn test_parse_ok(s: &str) {
    let parser = TermParser::new();
    assert!(parser.parse(s).is_ok());
}


#[test_case("22a"; "Number with a char is not a valid term.")]
#[test_case("(22"; "Number with only one opening parenthesis is not a valid term.")]
#[test_case("22)"; "Number with only one closing parenthesis is not a valid term.")]
#[test_case(")))"; "No number but a few closing parenthesis is not a valid term.")]
#[test_case(""; "Empty string is an invalid term.")]
fn test_parse_err(s: &str) {
    let parser = TermParser::new();
    assert!(parser.parse(s).is_err());
}