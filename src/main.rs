mod model;
mod program;
mod interpreter;

use crate::{program::Program, model::{Statement, Expression}, interpreter::interpret};

fn main() {
    println!("Hello Wabbit!");

    let program = Program{ 
        source: "".to_string(), 
        model: vec![
            Statement::PrintStatement(
                Expression::Integer(3)
            ),
            Statement::PrintStatement(
                Expression::Integer(23)
            ),
            Statement::PrintStatement(
                Expression::Char('\n')
            ),
            Statement::PrintStatement(
                Expression::Char('a')
            ),
            Statement::PrintStatement(
                Expression::Char('a')
            ),
            Statement::PrintStatement(
                Expression::Float(3.2)
            ),            
            Statement::PrintStatement(
                Expression::Boolean(true)
            ),                     

        ],
        has_errors: false 
    };

    interpret(program);

}
