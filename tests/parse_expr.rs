use tigers::ast::{Expr, BinOpCode};
use tigers::tiger::*;
use test_case::test_case;



#[test_case(r#""s""#, Expr::ConstantString("s".into()); "simple char s")]
#[test_case(r#""123""#, Expr::ConstantString("123".into()); "simple nums double quoted are a string literal")]
#[test_case(r#"123"#, Expr::ConstantInteger("123".into()); "simple nums without double quote are integer literal.")]
#[test_case(r#"0"#, Expr::ConstantInteger("0".into()); "single 0 without double quotes is an integer literal.")]
#[test_case(r#"(0)"#, Expr::ConstantInteger("0".into()); "single 0 without double quotes is an integer literal even when parenthesized.")]
fn parse_expr_constant(s: &str, expected: Expr) {
    let parser = ExprParser::new();
    let mut errors = vec![];
    let observed = parser.parse(&mut errors, s).unwrap();
    assert_eq!(*observed, expected);
}


#[test]
fn parse_expr_nil() {
    let parser = ExprParser::new();
    let mut errors = vec![];
    assert_eq!(*parser.parse(&mut errors, "nil").unwrap(), Expr::Nil);
}


#[test]
fn parse_expr_neg() {
    let parser = ExprParser::new();
    let mut errors =vec![];
    let neg_2: Expr = *parser.parse(&mut errors, "-2").unwrap();
    
    assert_eq!(neg_2, Expr::Neg(Box::new(Expr::ConstantInteger("2".to_string()))));
}


macro_rules! test_parse_ok {
    ($fn_name: ident, $parser_name:ident, $string:literal) => {
        #[test]
        fn $fn_name() {
            let parser = tigers::tiger::$parser_name::new();
            let mut errors = vec![];
            let parsed = parser.parse(&mut errors, $string).unwrap();
            assert!(errors.is_empty());
            println!("{:#?}", *parsed);
        }
    };

    ($fn_name: ident, $parser_name:ident, $string:literal, $op_code: expr) => {
        #[test]
        fn $fn_name() {
            let parser = tigers::tiger::$parser_name::new();
            let mut errors = vec![];
            let parsed = parser.parse(&mut errors, $string).unwrap();
            assert!(errors.is_empty());

            println!("{:#?}", *parsed);

            assert!(matches!(*parsed, Expr::BinOp(..)));
            let Expr::BinOp(_, observed_op_code, _) = *parsed else {
                panic!("Not a bin op.");
            };
            assert_eq!(observed_op_code, $op_code);
            
        }
    };

}


test_parse_ok!(lvalue_id, LValueParser, "abcde");
test_parse_ok!(lvalue_field_access, LValueParser, "abcde.fgh");
test_parse_ok!(lvalue_array_access, LValueParser, "abcde[\"fgh\"]");


test_parse_ok!(assign_op, ExprParser, r#"b1 := "abc""#);
// test_parse_ok!(bin_or_op, BinOrParser, r#"1 | "abc""#, OpCode::Or);
// test_parse_ok!(bin_and_op, BinAndParser, r#"1 & "abc""#, OpCode::And);
// test_parse_ok!(comparison_lte, ComparisonParser, r#"1 <= "abc""#, OpCode::LessThanEqual);
// test_parse_ok!(comparison_gte, ComparisonParser, r#"1 >= "abc""#, OpCode::GreaterThanEqual);
// test_parse_ok!(comparison_gt, ComparisonParser, r#"1 > "abc""#, OpCode::GreaterThan);
// test_parse_ok!(comparison_lt, ComparisonParser, r#"1 < "abc""#, OpCode::LessThan);
// test_parse_ok!(comparison_ltgt, ComparisonParser, r#"1 <> "abc""#, OpCode::LessThanGreaterThan);
// test_parse_ok!(comparison_eq, ComparisonParser, r#"1 = "abc""#, OpCode::Equal);

macro_rules! test_parse_keywords {
    ($($kw:ident),*) => {
        $(
            #[test]
            fn $kw() {
                let parser = tigers::tiger::KeywordParser::new();
                let mut errors = vec![];
                let string = stringify!($kw);
                if string.starts_with("r#") {
                    let substr = string[2..].to_owned();
                    let parsed = parser.parse(&mut errors, &substr).unwrap();
                    assert!(errors.is_empty());
                    println!("{:#?}", parsed);
                } else {
                    let parsed = parser.parse(&mut errors, string).unwrap();
                    assert!(errors.is_empty());
                    println!("{:#?}", parsed);
                };
            }
        )*
        
    };
}

test_parse_keywords! {
    array,
    r#break,
    r#do,
    r#else,
    end,
    r#for,
    function,
    r#if,
    r#in,
    r#let,
    nil,
    of,
    then,
    to,
    r#type,
    var,
    r#while
}

#[test]
fn parse_assign_expr() {

    // let parser = AssignParser::new();
    // let mut errors =vec![];
    // let exp = *parser.parse(&mut errors, r#"1:="abc""#).unwrap();
    
    // println!("{:#?}", exp);
    // println!("{:#?}", errors);


    // let parser = BinOrParser::new();
    // let mut errors =vec![];
    // let exp = *parser.parse(&mut errors, r#"1 | "abc""#).unwrap();
    
    // println!("{:#?}", exp);
    // println!("{:#?}", errors);


    // let parser = ComparisonParser::new();
    // let mut errors =vec![];
    // let exp = *parser.parse(&mut errors, r#"1 <= "abc""#).unwrap();
    
    // println!("{:#?}", exp);
    // println!("{:#?}", errors);


    // let parser = FactorParser::new();
    // let mut errors =vec![];
    // let exp = *parser.parse(&mut errors, r#"1 * """#).unwrap();
    
    // println!("{:#?}", exp);
    // println!("{:#?}", errors);

}


#[test]
fn parse_smol() {
    let smol = include_str!("smol.tig");
    let mut errors = vec![];

    let parser = DeclarationParser::new();
    let parsed = parser.parse(&mut errors, smol).unwrap();
    println!("smol: {:#?}", parsed);
    println!("errors: {:#?}", errors);
    assert_eq!(errors.len(), 0);
}

#[test]
fn parse_declarations() {

    let mut errors = vec![];
    let parser = DeclarationsParser::new();
    let parsed = parser.parse(&mut errors, "function foo() = 2\n function bar() = 3").unwrap();

    println!("smol: {:#?}", parsed);
    println!("errors: {:#?}", errors);
    assert_eq!(errors.len(), 0);
}

test_parse_ok!(type_only_type_id_single_char, TypeParser, "f");
test_parse_ok!(type_only_type_id, TypeParser, "foo");
test_parse_ok!(type_array_of, TypeParser, "array \nof int");
test_parse_ok!(type_fields, TypeParser, "{ x: int,\n y: string }");

test_parse_ok!(variable_decl_untyped_parser, VariableDeclParser, r#"var foo := "2""#);
test_parse_ok!(variable_decl_typed_parser, VariableDeclParser, r#"var foo: string := "2""#);
test_parse_ok!(declaration_variable_parser, DeclarationParser, r#"var foo: string := "2""#);
test_parse_ok!(declaration_type_parser, TypeParser, "array \nof int");
test_parse_ok!(declaration_function_return_untyped, DeclarationParser, "function foo() = 2");
test_parse_ok!(declaration_function_return_typed, DeclarationParser, "function foo(x: int): string = 2");
test_parse_ok!(type_decl_parser, TypeDeclParser, r#"type intArray = array of int"#);
