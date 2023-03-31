use comp_lib::{ast, diagnostic, passes};

fn parse(input: &str) -> diagnostic::AggregateResult<ast::Ast> {
    let cst = passes::parse::parse_to_cst(input);
    cst.and_then(|cst| passes::lower_cst::lower(&cst))
}

#[test]
fn parses_integer_literals() {
    macro_rules! check_literals_dec_ok {
        ($($($lit:expr),+ => $val:expr),+) => {
            $($(assert_expr_integer_lit($lit, ast::Literal::Dec($val));)+)+
        };
    }
    macro_rules! check_literals_hex_ok {
        ($($($lit:expr),+ => $val:expr),+) => {
            $($(assert_expr_integer_lit($lit, ast::Literal::Hex($val));)+)+
        };
    }
    macro_rules! check_literals_oct_ok {
        ($($($lit:expr),+ => $val:expr),+) => {
            $($(assert_expr_integer_lit($lit, ast::Literal::Octal($val));)+)+
        };
    }
    macro_rules! check_literals_err {
        ($($lit:expr),+) => {
            $(assert!(!parse(&format!("{};", $lit)).is_ok());)+
        };
    }
    // single-digit decimal literals
    check_literals_dec_ok! {
        "0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4,
        "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9
    };
    // single-digit octal literals
    check_literals_oct_ok! {
        "00" => 0, "01" => 1, "02" => 2, "03" => 3,
        "04" => 4, "05" => 5, "06" => 6, "07" => 7
    };
    // single-digit hexadecimal literals
    check_literals_hex_ok! {
        "0x0", "0X0" => 0x0,
        "0x1", "0X1" => 0x1,
        "0x2", "0X2" => 0x2,
        "0x3", "0X3" => 0x3,
        "0x4", "0X4" => 0x4,
        "0x5", "0X5" => 0x5,
        "0x6", "0X6" => 0x6,
        "0x7", "0X7" => 0x7,
        "0x8", "0X8" => 0x8,
        "0x9", "0X9" => 0x9,
        "0xa", "0Xa", "0xA", "0XA" => 0xa,
        "0xb", "0Xb", "0xB", "0XB" => 0xb,
        "0xc", "0Xc", "0xC", "0XC" => 0xc,
        "0xd", "0Xd", "0xD", "0XD" => 0xd,
        "0xe", "0Xe", "0xE", "0XE" => 0xe,
        "0xf", "0Xf", "0xF", "0XF" => 0xf
    };
    // regular numbers
    check_literals_dec_ok! {
        "1234" => 1234,
        "5678" => 5678,
        "100000" => 100000,
        "14581807" => 14581807
    }
    check_literals_oct_ok! {
        "0103" => 0o103,
        "07643" => 0o7643,
        "0230570" => 0o230570
    }
    check_literals_hex_ok! {
        "0x18bc9d3a" => 0x18bc9d3a,
        "0xDEADBEEF" => 0xdeadbeef,
        "0x9e3779b9" => 0x9e3779b9,
        "0X12345678" => 0x12345678
    };
    // superfluous zero-prefixes
    check_literals_oct_ok! {
        "003" => 3,
        "0001" => 1,
        "00007" => 7
    };
    check_literals_hex_ok! {
        "0x02" => 0x2,
        "0x0000010" => 0x10,
        "0x0b0a0" => 0xb0a0
    };
    // long but valid numbers
    check_literals_oct_ok! {
        "00000000000000000000000000000000000000007" => 7
    }
    check_literals_hex_ok! {
        "0x0000000000000000000F0010001" => 0xf0010001,
        "0XEDAFBCDBCAE" => 0xedafbcdbcae
    }
    check_literals_dec_ok! {
        "157238493290" => 157238493290
    };
    // erroneous number literals
    // numbers with digits oustide base range
    check_literals_err!["12ab", "30f", "0190", "0baf", "08", "0xGHB", "0Xfoobar"];
}

#[test]
fn parses_float_literal() {
    macro_rules! check_literals_ok {
            ($($lit:expr),+) => {
                $(assert_expr_float_lit($lit, $lit.parse().unwrap());)+
            };
            ($($lit:expr => $val:expr),+) => {
                $(assert_expr_float_lit($lit, $val);)+
            }
        }
    macro_rules! check_literals_err {
            ($($lit:expr),+) => {
                $(assert!(!parse(&format!("{};", $lit)).is_ok());)+
            }
        }
    check_literals_ok! {
        "0." => 0., "1." => 1., "2." => 2., "3." => 3., "4." => 4.,
        "5." => 5., "6." => 6., "7." => 7., "8." => 8., "9." => 9.
    };
    check_literals_ok![".0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9"];
    check_literals_ok!["0.0", "0.132", "00.000", "1010.01", "123456.987654"];
    // with exponent without sign
    check_literals_ok![
        "0.e0",
        "00.00e000",
        "3.2e2",
        "2.e16",
        "1.234e5",
        "3.e100",
        "0.00007e5"
    ];
    // with exponent with `+` sign
    check_literals_ok!["0.e+0", "1.0e+10", "32.1230e+2", "0.00009e+5"];
    // with exponent with `-` sign
    check_literals_ok!["0.e-0", "1.0e-10", "87.2938e-3", "134560.e-8"];
    // erroneous float literals
    check_literals_err![
        "1e3", "0..3", "2..", "9e", ".3e", "6.e", "1.4e+", "1.0e-", "5e--3", "2e++1"
    ];
}

#[test]
fn parses_char_literals() {
    macro_rules! check_literals_ok {
            ($($($lit:expr),+ => $val:expr),+) => {
                $($(assert_expr_char_lit($lit, $val);)+)+
            };
        }
    macro_rules! check_literals_err {
            ($($lit:expr),+) => {
                $(assert!(!parse(&format!("'{}';", $lit)).is_ok());)+
            }
        }
    check_literals_ok! {
        // regular chars
        "a" => b'a' as i128,
        "b" => b'b' as i128,
        "c" => b'c' as i128,
        "x" => b'x' as i128,
        "y" => b'y' as i128,
        "z" => b'z' as i128,
        "!" => b'!' as i128,
        "&" => b'&' as i128,
        "*" => b'*' as i128,
        "," => b',' as i128,
        "/" => b'/' as i128,
        "~" => b'~' as i128,
        "^" => b'^' as i128,
        "%" => b'%' as i128,
        "\"" => b'"' as i128,
        // escaped chars
        "\\'" => b'\'' as i128,
        "\\\"" => b'\"' as i128,
        "\\?" => b'?' as i128,
        "\\\\" => b'\\' as i128,
        "\\a" => 0x07,
        "\\b" => 0x08,
        "\\f" => 0x0c,
        "\\n" => b'\n' as i128,
        "\\r" => b'\r' as i128,
        "\\t" => b'\t' as i128,
        "\\v" => 0x0b
    };
    // octal escape sequence
    check_literals_ok! {
        "\\0", "\\00", "\\000" => 0,
        "\\1", "\\01", "\\001" => 1,
        "\\2", "\\02", "\\002" => 2,
        "\\3", "\\03", "\\003" => 3,
        "\\4", "\\04", "\\004" => 4,
        "\\5", "\\05", "\\005" => 5,
        "\\6", "\\06", "\\006" => 6,
        "\\7", "\\07", "\\007" => 7
    };
    check_literals_ok! {
        "\\13" => 0o13,
        "\\20" => 0o20,
        "\\53" => 0o53,
        "\\67" => 0o67,
        "\\123" => 0o123,
        "\\232" => 0o232,
        "\\377" => 0o377
    };
    check_literals_ok! {
        "\\x0" => 0x0,
        "\\x1" => 0x1,
        "\\x2" => 0x2,
        "\\x3" => 0x3,
        "\\x4" => 0x4,
        "\\x5" => 0x5,
        "\\x6" => 0x6,
        "\\x7" => 0x7,
        "\\x8" => 0x8,
        "\\x9" => 0x9,
        "\\xa", "\\xA" => 0xa,
        "\\xb", "\\xB" => 0xb,
        "\\xc", "\\xC" => 0xc,
        "\\xd", "\\xD" => 0xd,
        "\\xe", "\\xE" => 0xe,
        "\\xf", "\\xF" => 0xf
    };
    // incomplete or invalid character literals
    check_literals_err!["\\", "'"];
    // invalid octal escape sequences (non-octal digits)
    check_literals_err!["\\38", "\\49", "\\88", "\\181", "\\987"];
    // out-of-range octal escape sequences
    check_literals_err!["\\400", "\\467", "\\503", "\\777"];
    // capital-X hex escape sequences
    check_literals_err![
        "0X0", "0X1", "0X2", "0X3", "0X4", "0X5", "0X6", "0X7", "0X8", "0X9", "0Xa", "0Xb", "0Xc",
        "0Xd", "0Xe", "0Xf", "0XA", "0XB", "0XC", "0XD", "0XE", "0XF"
    ];
}

#[test]
fn parses_identifiers() {
    macro_rules! check_idents_ok {
            ($($ident:expr),+ $(,)?) => {
                $(assert_expr_ident($ident, $ident);)+
            };
        }
    macro_rules! check_idents_err {
            ($($ident:expr),+ $(,)?) => {
                $(assert!(!parse(&format!("{};", $ident)).is_ok());)+
            };
        }
    check_idents_ok![
        "lorem",
        "ipsum",
        "a",
        "B",
        "numbers123",
        "_",
        "__",
        "_xyz",
        "_DEFH",
        "__foobar",
        "__JohnDoe",
        "___amet___",
        "_01234",
        "SCREAMING_SNAKE_CASE",
        "PascalCase",
        "snake_case",
        "lowerCamelCase",
        "Mixed_case",
        "ALLCAPS",
        "EUR90",
        "xDEADBEEF",
        "o77",
        "very_long_identifiers_are_allowed_and_more_readable_if_separated_by_underscores"
    ];
    check_idents_err!["invalid_$_ident", "€62", "with space is also not allowed",];
}

#[test]
fn parses_unary_expr() {
    macro_rules! check_unary_expr_ok {
        ($expr:expr => [$op:ident : $op_range:expr], [$id:literal : $id_range:expr]) => {
            assert_expr_eq(
                String::from($expr),
                ast::Expression::Unary(
                    ast::UnaryOperatorNode {
                        span: diagnostic::Span::from($op_range),
                        data: ast::UnaryOperator::$op,
                    },
                    Box::new(ast::ExpressionNode {
                        span: diagnostic::Span::from($id_range),
                        data: ast::Expression::Ident(ast::IdentNode {
                            span: diagnostic::Span::from($id_range),
                            data: $id.to_owned(),
                        }),
                    }),
                ),
            );
        };
    }
    // postfix operators
    check_unary_expr_ok!("foo++" => [DoublePlusPostfix: 3..5], ["foo": 0..3]);
    check_unary_expr_ok!("foo--" => [DoubleMinusPostfix: 3..5], ["foo": 0..3]);
    // prefix operators
    check_unary_expr_ok!("++foo" => [DoublePlusPrefix: 0..2], ["foo": 2..5]);
    check_unary_expr_ok!("--foo" => [DoubleMinusPrefix: 0..2], ["foo": 2..5]);
    check_unary_expr_ok!("!foo" => [Bang: 0..1], ["foo": 1..4]);
    check_unary_expr_ok!("+foo" => [Plus: 0..1], ["foo": 1..4]);
    check_unary_expr_ok!("-foo" => [Minus: 0..1], ["foo": 1..4]);
    check_unary_expr_ok!("~foo" => [Tilde: 0..1], ["foo": 1..4]);
    check_unary_expr_ok!("&foo" => [Ampersand: 0..1], ["foo": 1..4]);
    check_unary_expr_ok!("*foo" => [Star: 0..1], ["foo": 1..4]);
}

#[test]
fn parses_cast_expr() {
    assert!(!parse("()foo").is_ok());
    assert!(!parse("(int){foo}").is_ok());
    assert!(!parse("(const)bar").is_ok());
    assert_expr_eq(
        "(const int *const) foo".to_owned(),
        ast::Expression::Cast(
            ast::QualifiedTypeNode {
                span: (1..17).into(),
                data: ast::QualifiedType {
                    is_const: Some((12..17).into()),
                    inner: ast::UnqualifiedTypeNode {
                        span: (1..17).into(),
                        data: ast::UnqualifiedType::PointerType(Box::new(ast::QualifiedTypeNode {
                            span: (1..10).into(),
                            data: ast::QualifiedType {
                                is_const: Some((1..6).into()),
                                inner: ast::UnqualifiedTypeNode {
                                    span: (7..10).into(),
                                    data: ast::UnqualifiedType::PlainType(
                                        ast::PlainType::Primitive(ast::PrimitiveType::Int),
                                    ),
                                },
                            },
                        })),
                    },
                },
            },
            Box::new(ast::ExpressionNode {
                span: (19..22).into(),
                data: ast::Expression::Ident(ast::IdentNode {
                    span: (19..22).into(),
                    data: "foo".to_owned(),
                }),
            }),
        ),
    );
}

fn assert_expr_integer_lit(raw_int_lit: &str, expected: ast::Literal) {
    assert_expr_eq(
        raw_int_lit.to_string(),
        ast::Expression::Literal(ast::LiteralNode {
            span: (0..raw_int_lit.len()).into(),
            data: expected,
        }),
    );
}

fn assert_expr_float_lit(raw_float_lit: &str, expected_value: f64) {
    assert_expr_eq(
        raw_float_lit.to_string(),
        ast::Expression::Literal(ast::LiteralNode {
            span: (0..raw_float_lit.len()).into(),
            data: ast::Literal::Float(expected_value),
        }),
    );
}

fn assert_expr_char_lit(raw_inner_char_lit: &str, expected_value: i128) {
    assert_expr_eq(
        format!("'{}'", raw_inner_char_lit),
        ast::Expression::Literal(ast::LiteralNode {
            span: (0..(raw_inner_char_lit.len() + 2)).into(),
            data: ast::Literal::Char(expected_value),
        }),
    );
}

fn assert_expr_ident(raw_ident: &str, expected_ident: &str) {
    assert_expr_eq(
        raw_ident.to_string(),
        ast::Expression::Ident(ast::IdentNode {
            span: (0..raw_ident.len()).into(),
            data: expected_ident.to_owned(),
        }),
    )
}

fn assert_expr_eq(raw_expr: String, expected_expr: ast::Expression) {
    let input = raw_expr + ";";
    let res = parse(&input);
    let is_ok = res.is_ok();
    assert_eq!(
        res.into_value(),
        Some(ast::Ast {
            global: ast::BlockStatement(vec![ast::StatementNode {
                comments: None,
                span: (0..input.len()).into(),
                data: ast::Statement::Expression(ast::ExpressionNode {
                    span: (0..(input.len() - 1)).into(),
                    data: expected_expr
                }),
            }]),
        })
    );
    assert!(is_ok);
}
