use std::fmt;

use crate::program::Program;

pub enum DataType{
    Integer,
    Float,
    Char,
    Bool
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DataType::Integer => "int",
                DataType::Float => "float",
                DataType::Char => "char",
                DataType::Bool => "bool",
            }
        )
    }
}

pub enum Expression {
    Identifier(String),
    Integer(i32),
    Float(f64),
    Boolean(bool),
    Char(char),
    BinOp{lhs: Box<Expression>, op: Op, rhs: Box<Expression>},
    RelOp{lhs: Box<Expression>, op: RelOp, rhs:Box<Expression>},
    Grouping(Box<Expression>),
    Assignment{lhs: String, rhs: Box<Expression>},
    Block{body: Vec<Statement>},
}

pub enum Statement {
    PrintStatement(Expression),
    Expression(Expression),
    ConstDeclaration{name: String, value: Option<Expression> },
    VarDeclaration{name: String, data_type: Option<DataType>, value: Option<Expression> },
    If{condition: Expression, body: Vec<Statement>, else_body: Option<Vec<Statement>>},
    While{condition: Expression, body: Vec<Statement>},
    Break,
    Continue
}

pub enum Op {
    Add,
    Sub,
    Mult,
    Div
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add => "+",
                Op::Sub => "-",
                Op::Mult => "*",
                Op::Div => "/",
            }
        )
    }
}

pub enum RelOp {
    GT,
    GE,
    LT,
    LE,
    EQ,
    OR,
}

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RelOp::GT => ">",
                RelOp::GE => ">=",
                RelOp::LT => "<",
                RelOp::LE => "<=",
                RelOp::EQ => "==",
                RelOp::OR => "||",
            }
        )
    }
}


pub fn program_to_source(program: &Program<Statement>, indent: usize) -> String {
    let mut source_code: Vec<String> = Vec::new();
    for node in &program.model {
        let line = statement_to_source(&node, indent);
        source_code.push(line);
    }

    return source_code.join("\n");
}

pub fn expression_to_source(node: &Expression, indent: usize) -> String {
    match node {
        Expression::Identifier(name) => name.to_string(),
        Expression::Integer(i) => i.to_string(),
        Expression::Float(f) => format_float(&f),
        Expression::Boolean(b) => b.to_string(),
        Expression::Char(c) => format!("'{}'", c),
        Expression::BinOp { lhs, op, rhs } => format!(
            "{} {} {}", 
            expression_to_source(lhs, indent), 
            op,
            expression_to_source(rhs, indent)
        ),
        Expression::Grouping(expression) => format!(
            "({})",
            expression_to_source(expression, indent)
        ),
        Expression::Assignment { lhs, rhs } => format!(
            "{} = {};",
            lhs,
            expression_to_source(rhs, indent)
        ),
        Expression::RelOp { lhs, op, rhs } => format!(
            "{} {} {}",
            expression_to_source(lhs, indent),
            op.to_string(),
            expression_to_source(rhs, indent)
        ),
        Expression::Block { body } => {
            let mut source: String = String::new();
            for line in body {
                let s = format!("{}\n", statement_to_source(line, indent));
                source.push_str(&s)
            }
            return source;
        },
    }
}

pub fn statement_to_source(node: &Statement, indent: usize) -> String {

    let padding = "    ".repeat(indent);

    match node {
        Statement::PrintStatement(expression) => {
            return format!("{}print {};", padding, &expression_to_source(expression, indent));
        },
        Statement::Expression(expression) => {
            return format!("{}{}", padding, expression_to_source(expression, indent));
        },
        Statement::ConstDeclaration { name, value } => {
            match value {
                Some(value) => return format!("{}const {} = {};", padding, name, expression_to_source(value, indent)),
                None => return format!("{}const {};", padding, name),
            }
        },
        Statement::VarDeclaration { name, data_type, value } => {
            match (data_type, value) {
                (None, None) => panic!("Invalid. Var declaration must have data_type or value"),
                (None, Some(value)) => format!("{}var {} = {};", padding, name, expression_to_source(value, indent)),
                (Some(data_type), None) => format!("{}var {} {};", padding, name, data_type),
                (Some(data_type), Some(value)) => format!("{}var {} {} = {};", padding, name, data_type, expression_to_source(value, indent)),
            }

        },
        Statement::If { condition, body, else_body } => {
            let mut source = format!("{}if {} {{\n", padding, expression_to_source(condition, indent));
            for line in body {
                let s = format!("{}\n", statement_to_source(line, indent + 1));
                source.push_str(&s)
            }
            source.push_str(&format!("{}}}", padding));

            match else_body {
                Some(body) => {
                    let mut else_source: String = String::from(" else {\n");
                    for line in body {
                        let s = format!("{}\n", statement_to_source(line, indent + 1));
                        else_source.push_str(&s)
                    }
                    else_source.push_str(&format!("{}}}", padding));
                    source.push_str(&else_source);
                },
                None => (),
            };
            return source;
        },
        Statement::While { condition, body } => {
            let mut source = format!("{}while {} {{\n", padding, expression_to_source(condition, indent));
            for line in body {
                let s = format!("{}\n", statement_to_source(line, indent + 1));
                source.push_str(&s)
            }
            source.push_str(&format!("{}}}", padding));
            return source;
        },
        Statement::Break => format!("{}break;", padding),
        Statement::Continue => format!("{}continue;", padding),
    }
}

fn format_float(f: &f64) -> String {
    if f.fract() == 0.0 {
        format!("{:.1}", f)
    }
    else {
        format!("{:.}", f)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn get_wabbit_source(program_name: &str) -> String {
        let filepath = format!("src/wabbit_source/{}", program_name);
        let source = fs::read_to_string(filepath)
            .expect("Failed to open program1.wb");    
        return source;
    }

    #[test]
    fn program1() {   
        let source: String = get_wabbit_source("program1.wb");

        let program = Program{ 
            source: source.to_string(), 
            model: vec![
                Statement::PrintStatement(
                    Expression::Char('a')
                ),
                Statement::PrintStatement(
                    Expression::Integer(2)
                ),
                Statement::PrintStatement(
                    Expression::BinOp {
                        lhs: Box::new(Expression::Integer(2)), 
                        op: Op::Add, 
                        rhs: Box::new(Expression::Integer(3))
                    }
                ),
                Statement::PrintStatement(
                    Expression::BinOp {
                        lhs: Box::new(Expression::Integer(-2)), 
                        op: Op::Add, 
                        rhs: Box::new(Expression::Integer(3))
                    }
                ),
                Statement::PrintStatement(
                    Expression::BinOp{
                        lhs: Box::new(Expression::Integer(2)), 
                        op: Op::Add, 
                        rhs: Box::new(
                            Expression::BinOp {
                                lhs: Box::new(Expression::Integer(3)), 
                                op: Op::Mult, 
                                rhs: Box::new(Expression::Integer(-4))
                            }
                        )
                    }
                ),
                Statement::PrintStatement(
                    Expression::BinOp{
                        lhs: Box::new(
                            Expression::Grouping(
                                Box::new(
                                    Expression::BinOp {
                                        lhs: Box::new(Expression::Integer(2)), 
                                        op: Op::Add, 
                                        rhs: Box::new(Expression::Integer(3)) 
                                    }
                                )
                            )
                        ), 
                        op: Op::Mult, 
                        rhs: Box::new(Expression::Integer(-4)) 
                    }
                ),
                Statement::PrintStatement(
                    Expression::BinOp{
                        lhs: Box::new(Expression::Float(2.0)), 
                        op: Op::Sub, 
                        rhs: Box::new(
                            Expression::BinOp {
                                lhs: Box::new(Expression::Float(3.0)), 
                                op: Op::Div, 
                                rhs: Box::new(Expression::Float(-4.0))
                            }
                        )
                    }
                ),       
    
            ],
            has_errors: false 
        };
    
        let s = program_to_source(&program, 0);
        assert_eq!(program.source, s);    
        println!("\n--Program1--\n{}\n", s);
    }

    #[test]
    fn program2() {
        let source: String = get_wabbit_source("program2.wb");

        let program = Program{ 
            source: source.to_string(), 
            model: vec![
                Statement::ConstDeclaration {
                    name: String::from("pi"), 
                    value: Some(Expression::Float(3.14159))
                },
                Statement::ConstDeclaration {
                    name: String::from("tau"), 
                    value: Some(
                        Expression::BinOp{
                            lhs: Box::new(Expression::Float(2.0)),
                            op: Op::Mult,
                            rhs: Box::new(Expression::Identifier("pi".to_string()))
                        }
                    )
                },
                Statement::VarDeclaration {
                    name: String::from("radius"), 
                    data_type: None,
                    value: Some(
                        Expression::Float(4.0)
                    )
                },
                Statement::VarDeclaration {
                    name: String::from("perimeter"), 
                    data_type: Some(DataType::Float),
                    value: None
                },
                Statement::Expression(
                    Expression::Assignment {
                        lhs: "perimeter".to_string(), 
                        rhs:  Box::new(
                            Expression::BinOp {
                                lhs: Box::new(Expression::Identifier("tau".to_string())), 
                                op: Op::Mult, 
                                rhs: Box::new(Expression::Identifier("radius".to_string())) 
                            }
                        )
                    }
                ),
                Statement::PrintStatement(Expression::Identifier("perimeter".to_string()))
            ],
            has_errors: false 
        };
    
        let s = program_to_source(&program, 0);
        assert_eq!(program.source, s);    
        println!("\n--Program2--\n{}\n", s);
    }

    #[test]
    fn program3() {
        let source: String = get_wabbit_source("program3.wb");

        let program = Program{ 
            source: source.to_string(), 
            model: vec![
                Statement::PrintStatement(
                    Expression::RelOp{
                        lhs: Box::new(Expression::Integer(1)), 
                        op: RelOp::EQ, 
                        rhs: Box::new(Expression::Integer(1)) 
                    }
                ),
                Statement::PrintStatement(
                    Expression::RelOp{
                        lhs: Box::new(Expression::Integer(0)), 
                        op: RelOp::LT, 
                        rhs: Box::new(Expression::Integer(1)) 
                    }
                ),
                Statement::PrintStatement(
                    Expression::RelOp{
                        lhs: Box::new(
                            Expression::RelOp {
                                lhs: Box::new(Expression::Integer(0)), 
                                op: RelOp::LT, 
                                rhs: Box::new(Expression::Integer(1))
                            }
                        ), 
                        op: RelOp::LT, 
                        rhs: Box::new(Expression::Integer(2)) 
                    }
                ),
                Statement::PrintStatement(
                    Expression::RelOp {
                        lhs: Box::new(Expression::Boolean(true)), 
                        op: RelOp::OR, 
                        rhs: Box::new(
                            Expression::Grouping(
                                Box::new(
                                    Expression::RelOp {
                                        lhs: Box::new(
                                            Expression::BinOp {
                                                lhs: Box::new(Expression::Integer(1)), 
                                                op: Op::Div, 
                                                rhs: Box::new(Expression::Integer(0)) 
                                            }
                                        ), 
                                        op: RelOp::EQ, 
                                        rhs: Box::new(Expression::Integer(0))
                                    }
                                )
                            )
                        ) 
                    }
                )

            ],
            has_errors: false 
        };
    
        let s = program_to_source(&program, 0);
        assert_eq!(program.source, s);    
        println!("\n--Program3--\n{}\n", s);
    }

    #[test]
    fn program4() {
        let source: String = get_wabbit_source("program4.wb");

        let program = Program{ 
            source: source.to_string(), 
            model: vec![
                Statement::VarDeclaration {
                    name: "a".to_string(), 
                    data_type: Some(DataType::Integer), 
                    value: Some(Expression::Integer(2))
                },
                Statement::VarDeclaration {
                    name: "b".to_string(), 
                    data_type: Some(DataType::Integer), 
                    value: Some(Expression::Integer(3))
                },
                Statement::VarDeclaration {
                    name: "minval".to_string(), 
                    data_type: Some(DataType::Integer), 
                    value: None
                },
                Statement::If {
                    condition: Expression::RelOp {
                        lhs: Box::new(Expression::Identifier("a".to_string())), 
                        op: RelOp::LT, 
                        rhs: Box::new(Expression::Identifier("b".to_string())) 
                    }
                    ,
                    body: vec![
                        Statement::Expression(
                            Expression::Assignment {
                                lhs: "minval".to_string(), 
                                rhs: Box::new(Expression::Identifier("a".to_string())) 
                            }
                        )
                    ],
                    else_body: Some(
                        vec![
                            Statement::Expression(
                                Expression::Assignment {
                                    lhs: "minval".to_string(), 
                                    rhs: Box::new(Expression::Identifier("b".to_string())) 
                                }
                            )
                        ]
                    ) 
                },
                Statement::PrintStatement(
                    Expression::Identifier("minval".to_string())
                )
            ],
            has_errors: false 
        };

        let s = program_to_source(&program, 0);
        assert_eq!(program.source, s);
        println!("\n--Program4--\n{}\n", s);
    }

    #[test]
    fn program5() {
        let source: String = get_wabbit_source("program5.wb");

        let program = Program{ 
            source: source.to_string(), 
            model: vec![
                Statement::ConstDeclaration {
                    name: "n".to_string(), 
                    value: Some(Expression::Integer(10))
                },
                Statement::VarDeclaration {
                    name: "x".to_string(), 
                    data_type: Some(DataType::Integer), 
                    value: Some(Expression::Integer(1))
                },
                Statement::VarDeclaration {
                    name: "fact".to_string(), 
                    data_type: Some(DataType::Integer), 
                    value: Some(Expression::Integer(1))
                },
                Statement::While {
                    condition: Expression::RelOp {
                        lhs: Box::new(Expression::Identifier("x".to_string())), 
                        op: RelOp::LT, 
                        rhs: Box::new(Expression::Identifier("n".to_string())) 
                    }, 
                    body: vec![
                        Statement::Expression(
                            Expression::Assignment {
                                lhs: "fact".to_string(), 
                                rhs: Box::new(
                                    Expression::BinOp {
                                        lhs: Box::new(
                                            Expression::Identifier("fact".to_string())
                                        ), 
                                        op: Op::Mult, 
                                        rhs: Box::new(
                                            Expression::Identifier("x".to_string())
                                        ) 
                                    }
                                ) 
                            }
                        ),
                        Statement::Expression(
                            Expression::Assignment {
                                lhs: "x".to_string(), 
                                rhs: Box::new(
                                    Expression::BinOp {
                                        lhs: Box::new(
                                            Expression::Identifier("x".to_string())
                                        ), 
                                        op: Op::Add, 
                                        rhs: Box::new(
                                            Expression::Integer(1)
                                        ) 
                                    }
                                ) 
                            }
                        ),
                        Statement::PrintStatement(Expression::Identifier("fact".to_string()))                 
                    ] 
                }
            ],
            has_errors: false 
        };

        let s = program_to_source(&program, 0);
        assert_eq!(program.source, s);
        println!("\n--Program5--\n{}\n", s);
    }

    #[test]
    fn program6() {
        let source: String = get_wabbit_source("program6.wb");

        let program = Program{
            source: source.to_string(),
            model: vec![
                Statement::VarDeclaration{
                    name: "n".to_string(),
                    data_type: None,
                    value: Some(Expression::Integer(0))
                },
                Statement::While {
                    condition: Expression::Boolean(true), 
                    body: vec![
                        Statement::If {
                            condition: Expression::RelOp { 
                                lhs: Box::new(Expression::Identifier("n".to_string())), 
                                op: RelOp::EQ, 
                                rhs: Box::new(Expression::Integer(2)) 
                            }, 
                            body: vec![
                                Statement::PrintStatement(Expression::Identifier("n".to_string())),
                                Statement::Break
                            ], 
                            else_body: Some(vec![
                                Statement::Expression(
                                    Expression::Assignment {
                                        lhs: "n".to_string(),
                                        rhs: Box::new(Expression::BinOp {
                                            lhs: Box::new(Expression::Identifier("n".to_string())),
                                            op: Op::Add,
                                            rhs: Box::new(Expression::Integer(1))
                                        })
                                    }
                                ),
                                Statement::Continue
                            ])
                        },
                        Statement::Expression(
                            Expression::Assignment {
                                lhs: "n".to_string(),
                                rhs: Box::new(Expression::BinOp {
                                    lhs: Box::new(Expression::Identifier("n".to_string())),
                                    op: Op::Sub,
                                    rhs: Box::new(Expression::Integer(1))
                                })
                            }
                        ),
                    ] 
                }
            ],
            has_errors: false 
        };

        let s = program_to_source(&program, 0);
        assert_eq!(program.source, s);
        println!("\n--Program5--\n{}\n", s);
    }


}