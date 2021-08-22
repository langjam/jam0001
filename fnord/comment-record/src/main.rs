mod ast;
mod env;
mod parse;
mod types;
mod values;

fn main() {
    let script = parse::parse(
        "test_script",
        "
# A number obtained by taking the ratio between two integers.
# DISPLAY: number
struct Rational {
    # The number on top
    # UNIT: unitless
    numerator: Number,
    denominator: Number,
};

# The ratio between the circumference of a circle and the diameter.
# VERIFIED: true
# SOURCE: divine inspiration
pi = Rational {
    # nice and big
    numerator: 54,
    # not quite as big
    denominator: 17,
};

result = pi.numerator;
picomment = pi!!;
verified = pi!VERIFIED;

some_text = pi!text;

swapped = Rational {
    #! pi.numerator
    numerator: pi.denominator,
    #! pi.denominator
    denominator: pi.numerator,
};
",
    ).unwrap();
    /*
    let script = ast::Script {
        name: "test script".into(),
        statements: vec![
            ast::Ast::TypeDef(
                ast::StructType::from(ast::StructTypeData {
                    name: Some("Rational".into()),
                    fields: vec![
                        ast::FieldType {
                            name: "numerator".into(),
                            ty: ast::TypeAst::Named("Number".into()),
                            comment: ast::Comment {
                                lines: vec![
                                    "The number on top.".into(),
                                ],
                            },
                        },
                        ast::FieldType {
                            name: "denominator".into(),
                            ty: ast::TypeAst::Named("Number".into()),
                            comment: ast::Comment {
                                lines: vec![],
                            },
                        },
                    ],
                    comment: ast::Comment {
                        lines: vec![
                            "A number obtained by taking the ratio between two integers.".into(),
                            "UNIT: %".into(),
                        ],
                    },
                }),
            ),
            ast::Ast::ValueDef(
                "pi".into(),
                ast::ValueAst::Struct(ast::StructValue::from(ast::StructValueData {
                    name: Some("Rational".into()),
                    fields: vec![
                        ast::FieldValue {
                            name: "numerator".into(),
                            value: ast::ValueAst::Number(54),
                            comment: ast::Comment {
                                lines: vec![],
                            },
                        },
                        ast::FieldValue {
                            name: "denominator".into(),
                            value: ast::ValueAst::Number(17),
                            comment: ast::Comment {
                                lines: vec![],
                            },
                        },
                    ],
                    comment: ast::Comment {
                        lines: vec![
                            "VERIFIED: true".into(),
                            "SOURCE: divine inspiration".into(),
                        ],
                    },
                })),
            ),
            ast::Ast::ValueDef(
                "result".into(),
                ast::ValueAst::FieldAccess(
                    Box::new(ast::ValueAst::Reference("pi".into())),
                    "numerator".into(),
                ),
            ),
            ast::Ast::ValueDef(
                "verified".into(),
                ast::ValueAst::CommentAccess(
                    Box::new(ast::ValueAst::Reference("pi".into())),
                    "VERIFIED".into(),
                ),
            ),
        ],
    };
*/
    println!("{:?}", script);

    let mut env = env::Env::new();

    let result = env.run_script(&script);

    if let Some(value) = result {
        println!(" => {:?}", value);
    }
}