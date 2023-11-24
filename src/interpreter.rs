use std::io::{self, Write};

use crate::{program::Program, model::{Statement, Expression, DataType}};

pub struct ExpressionResult {
    data_type: DataType,
    value: String
}


pub fn interpret(program: Program<Statement>) {
    for statement in program.model {
        intepret_statement(statement);
    }
}


pub fn intepret_statement(node: Statement) {
    match node {
        Statement::PrintStatement(expression) => {
            let result = interpret_expression(&expression);

            match expression {
                Expression::Integer(_) | 
                Expression::Char(_) |
                Expression::Float(_) => print!("{}", result.value),
                _ => println!("{}", result.value)
            }
            io::stdout().flush().unwrap();
        },
        Statement::Expression(_) => todo!(),
        Statement::ConstDeclaration { name, value } => todo!(),
        Statement::VarDeclaration { name, data_type, value } => todo!(),
        Statement::FuncDeclaration { name, params, return_type, body } => todo!(),
        Statement::If { condition, body, else_body } => todo!(),
        Statement::While { condition, body } => todo!(),
        Statement::Break => todo!(),
        Statement::Continue => todo!(),
        Statement::Return(_) => todo!(),
        Statement::BlankLine => todo!(),
    }
}


pub fn interpret_expression(node: &Expression) -> ExpressionResult {
    match node {
        Expression::Identifier(i) => ExpressionResult{ data_type: DataType::Integer, value: i.to_string() },
        Expression::Integer(i) => ExpressionResult{ data_type: DataType::Integer, value: i.to_string() },
        Expression::Float(f) => ExpressionResult{ data_type: DataType::Integer, value: f.to_string() },
        Expression::Boolean(b) => ExpressionResult{ data_type: DataType::Integer, value: b.to_string() },
        Expression::Char(c) => ExpressionResult{ data_type: DataType::Char, value: c.to_string() },

        Expression::BinOp { lhs, op, rhs } => todo!(),
        Expression::RelOp { lhs, op, rhs } => todo!(),
        Expression::Grouping(_) => todo!(),
        Expression::Assignment { lhs, rhs } => todo!(),
        Expression::Block { body } => todo!(),
        Expression::FunctionCall { name, body } => todo!(),
    }
}

