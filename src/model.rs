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
    Assignment{lhs: String, rhs: Box<Expression>}
}

pub enum Statement {
    PrintStatement(Expression),
    Expression(Expression),
    ConstDeclaration{name: String, value: Option<Expression> },
    VarDeclaration{name: String, data_type: Option<DataType>, value: Option<Expression> }
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
        Expression::Char(_) => todo!(),
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

}