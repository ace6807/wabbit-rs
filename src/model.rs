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
    If{condition: Box<Expression>, body: Vec<Statement>, else_body: Option<Vec<Statement>>},
}

pub enum Op {
    Add,
    Sub,
    Mult,
    Div
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


pub fn program_to_source(program: &Program<Statement>) -> String {
    let mut source_code: Vec<String> = Vec::new();
    for node in &program.model {
        let line = statement_to_source(&node);
        source_code.push(line);
    }

    return source_code.join("\n");
}

pub fn expression_to_source(node: &Expression) -> String {
    match node {
        Expression::Identifier(name) => name.to_string(),
        Expression::Integer(i) => i.to_string(),
        Expression::Float(f) => format_float(&f),
        Expression::Boolean(b) => b.to_string(),
        Expression::Char(c) => format!("'{}'", c),
        Expression::BinOp { lhs, op, rhs } => format!(
            "{} {} {}", 
            expression_to_source(lhs), 
            op_to_source(op),
            expression_to_source(rhs)
        ),
        Expression::Grouping(expression) => format!(
            "({})",
            expression_to_source(expression)
        ),
        Expression::Assignment { lhs, rhs } => format!(
            "{} = {};",
            lhs,
            expression_to_source(rhs)
        ),
        Expression::RelOp { lhs, op, rhs } => format!(
            "{} {} {}",
            expression_to_source(lhs),
            op.to_string(),
            expression_to_source(rhs)
        ),
        Expression::Block { body } => {
            let mut source: String = String::new();
            for line in body {
                let s = format!("{}\n", statement_to_source(line));
                source.push_str(&s)
            }
            return source;
        },
    }
}

pub fn statement_to_source(node: &Statement) -> String {
    match node {
        Statement::PrintStatement(expression) => {
            return String::from("print ") + &expression_to_source(expression) + &";".to_string();
        },
        Statement::Expression(expression) => {
            return expression_to_source(expression);
        },
        Statement::ConstDeclaration { name, value } => {
            match value {
                Some(value) => return format!("const {} = {};", name, expression_to_source(value)),
                None => return format!("const {};", name),
            }
        },
        Statement::VarDeclaration { name, data_type, value } => {
            match (data_type, value) {
                (None, None) => panic!("Invalid. Var declaration must have data_type or value"),
                (None, Some(value)) => format!("var {} = {};", name, expression_to_source(value)),
                (Some(data_type), None) => format!("var {} {};", name, data_type),
                (Some(data_type), Some(value)) => format!("var {} {} = {};", name, data_type, expression_to_source(value)),
            }

        },
        Statement::If { condition, body, else_body } => {
            let mut source = format!("if {} {{\n", expression_to_source(condition));
            for line in body {
                let s = format!("{}\n", statement_to_source(line));
                source.push_str(&s)
            }
            source.push_str("}");

            match else_body {
                Some(body) => {
                    let mut else_source: String = String::from(" else {\n");
                    for line in body {
                        let s = format!("{}\n", statement_to_source(line));
                        else_source.push_str(&s)
                    }
                    else_source.push_str("}");
                    source.push_str(&else_source);
                },
                None => (),
            };
            return source;
        },
    }
}

pub fn op_to_source(op: &Op) -> String{
    match op {
        Op::Add => return String::from("+"),
        Op::Sub => return String::from("-"),
        Op::Mult => return String::from("*"),
        Op::Div => return String::from("/"),
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

    #[test]
    fn program1() {
        let source = "\
        print 2;\n\
        print 2 + 3;\n\
        print -2 + 3;\n\
        print 2 + 3 * -4;\n\
        print (2 + 3) * -4;\n\
        print 2.0 - 3.0 / -4.0;";
    
    
        let program = Program{ 
            source: source.to_string(), 
            model: vec![
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
    
        let s = program_to_source(&program);
        assert_eq!(program.source, s);    
        println!("\n--Program1--\n{}\n", s);
    }

    #[test]
    fn program2() {
        let source = "\
        const pi = 3.14159;\n\
        const tau = 2.0 * pi;\n\
        var radius = 4.0;\n\
        var perimeter float;\n\
        perimeter = tau * radius;\n\
        print perimeter;";

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
    
        let s = program_to_source(&program);
        assert_eq!(program.source, s);    
        println!("\n--Program2--\n{}\n", s);
    }

    #[test]
    fn program3() {
        let source = "\
        print 1 == 1;\n\
        print 0 < 1;\n\
        print 0 < 1 < 2;\n\
        print true || (1 / 0 == 0);";

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
    
        let s = program_to_source(&program);
        assert_eq!(program.source, s);    
        println!("\n--Program3--\n{}\n", s);
    }

    #[test]
    fn program4() {
        let source = "\
        var a int = 2;\n\
        var b int = 3;\n\
        var minval int;\n\
        if a < b {\n\
            minval = a;\n\
        } else {\n\
            minval = b;\n\
        }";

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
                    condition: Box::new(
                        Expression::RelOp {
                            lhs: Box::new(Expression::Identifier("a".to_string())), 
                            op: RelOp::LT, 
                            rhs: Box::new(Expression::Identifier("b".to_string())) 
                        }
                    ),
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
                }

            ],
            has_errors: false 
        };

        let s = program_to_source(&program);
        assert_eq!(program.source, s);
        println!("\n--Program4--\n{}\n", s);
    }
}