use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::EOF,
            next_token: Token::EOF,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn current_token_is(&mut self, tok: Token) -> bool {
        self.current_token == tok
    }

    fn next_token_is(&mut self, tok: &Token) -> bool {
        self.next_token == *tok
    }

    fn expect_next_token(&mut self, tok: Token) -> bool {
        if self.next_token_is(&tok) {
            self.next_token();
            return true;
        } else {
            panic!("expected next token to be {:?}, got {:?} instead", tok, self.next_token);
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program: Program = vec![];
        while !self.current_token_is(Token::EOF) {
            match self.parse_stmt() {
                Some(stmt) => program.push(stmt),
                None => {}
            }
            self.next_token();
        }
        program
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.current_token {
            Token::LET => self.parse_let_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        match &self.next_token {
            Token::IDENT(_) => self.next_token(),
            _ => panic!("expected next token to be IDENT, got {:?} instead", self.next_token)
        }

        let name = match self.parse_ident() {
            Some(name) => name,
            None => return None,
        };

        if !self.expect_next_token(Token::ASSIGN) {
            return None;
        }
        self.next_token();
        let expr = match self.parse_expr() {
            Some(expr) => expr,
            None => return None,
        };
        if self.next_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        Some(Stmt::Let(name, expr))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        match self.parse_expr() {
            Some(expr) => {
                if self.next_token_is(&Token::SEMICOLON) {
                    self.next_token();
                }
                Some(Stmt::Expr(expr))
            }
            None => None,
        }
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        let left = match self.current_token {
            Token::INT(_) => self.parse_int_expr(),
            _ => None,
        };
        left
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        match self.current_token {
            Token::IDENT(ref mut ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }

    fn parse_int_expr(&mut self) -> Option<Expr> {
        match self.current_token {
            Token::INT(v) => Some(Expr::Literal(Literal::Int(v.clone()))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::*;

    #[test]
    fn test_let_stmt() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        print!("{:?}", program);

        assert_eq!(
            vec![
                Stmt::Let(Ident(String::from("x")), Expr::Literal(Literal::Int(5))),
                Stmt::Let(Ident(String::from("y")), Expr::Literal(Literal::Int(10))),
                Stmt::Let(
                    Ident(String::from("foobar")),
                    Expr::Literal(Literal::Int(838383)),
                ),
            ],
            program,
        );
    }
}