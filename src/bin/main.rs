use rustyline::error::ReadlineError;
use rustyline::Editor;

use lang::lexer::Lexer;
use lang::token::Token;

fn main() {
    let mut rl = Editor::<()>::new();
    println!("Hello! Toy language!");
    println!("Author: hadxu");
    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                // println!("{}", line);
                let mut lexer = Lexer::new(&line);
                loop {
                    let tok = lexer.next_token();
                    if tok != Token::EOF {
                        println!("{:?}", tok);
                    } else {
                        break;
                    }
                }
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
