use rustyline::error::ReadlineError;
use rustyline::Editor;


use lang::evaluator::Evaluator;
use lang::lexer::Lexer;
use lang::parser::Parser;
use lang::evaluator::env::Env;
use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    let mut rl = Editor::<()>::new();
    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));

    println!("Hello! Toy language!");
    println!("Author: hadxu");
    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                // println!("{}", line);
                let lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);
                let program = parser.parse();

                if let Some(evaluated) = evaluator.eval(program) {
                    println!("{}", evaluated);
                }

                // loop {
                //     let tok = lexer.next_token();
                //     if tok != Token::EOF {
                //         println!("{:?}", tok);
                //     } else {
                //         break;
                //     }
                // }
            }
            Err(ReadlineError::Interrupted) => {
                println!("\nBye :)");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
            }
        }
    }
}
