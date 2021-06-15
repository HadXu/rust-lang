use crate::lexer::Lexer;
use crate::token::Token;
use crate::{ast::*, lexer};

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

    fn token_to_precedence(tok: &Token) -> Precedence {
        match tok {
            Token::EQ | Token::NOT_EQ => Precedence::EQUALS,
            Token::LT | Token::GT => Precedence::LESSGREATER,
            Token::PLUS | Token::MINUS => Precedence::SUM,
            Token::SLASH | Token::ASTERISK => Precedence::PRODUCT,
            Token::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }

    fn current_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(&self.current_token)
    }

    fn next_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(&self.next_token)
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
            panic!(
                "expected next token to be {:?}, got {:?} instead",
                tok, self.next_token
            );
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
            Token::RETURN => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        match &self.next_token {
            Token::IDENT(_) => self.next_token(),
            _ => panic!(
                "expected next token to be IDENT, got {:?} instead",
                self.next_token
            ),
        }

        let name = match self.parse_ident() {
            Some(name) => name,
            None => return None,
        };

        if !self.expect_next_token(Token::ASSIGN) {
            return None;
        }
        self.next_token();
        let expr = match self.parse_expr(Precedence::LOWEST) {
            Some(expr) => expr,
            None => return None,
        };
        if self.next_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        Some(Stmt::Let(name, expr))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token();
        let expr = match self.parse_expr(Precedence::LOWEST) {
            Some(expr) => expr,
            None => return None,
        };

        if self.next_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        Some(Stmt::Return(expr))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        match self.parse_expr(Precedence::LOWEST) {
            Some(expr) => {
                if self.next_token_is(&Token::SEMICOLON) {
                    self.next_token();
                }
                Some(Stmt::Expr(expr))
            }
            None => None,
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        // prefix
        let mut left = match self.current_token {
            Token::IDENT(_) => self.parse_ident_expr(),
            Token::INT(_) => self.parse_int_expr(),
            Token::BANG | Token::MINUS | Token::PLUS => self.parse_prefix_expr(),
            Token::LPAREN => self.parse_grouped_expr(),
            Token::BOOL(_) => self.parse_bool_expr(),
            Token::IF => self.parse_if_expr(),
            Token::FUNCTION => self.parse_func_expr(),
            _ => panic!("do not support {:?} token", self.current_token),
        };

        // infix
        while !self.next_token_is(&Token::SEMICOLON) && precedence < self.next_token_precedence() {
            match self.next_token {
                Token::PLUS
                | Token::MINUS
                | Token::SLASH
                | Token::ASTERISK
                | Token::EQ
                | Token::NOT_EQ
                | Token::LT
                | Token::GT => {
                    self.next_token();
                    left = self.parse_infix_expr(left.unwrap());
                }
                Token::LPAREN => {
                    self.next_token();
                    left = self.parse_call_expr(left.unwrap());
                }
                _ => return left,
            }
        }
        left
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        match self.current_token {
            Token::IDENT(ref mut ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }

    fn parse_ident_expr(&mut self) -> Option<Expr> {
        match self.parse_ident() {
            Some(ident) => Some(Expr::Ident(ident)),
            None => None,
        }
    }

    fn parse_int_expr(&mut self) -> Option<Expr> {
        match self.current_token {
            Token::INT(v) => Some(Expr::Literal(Literal::Int(v.clone()))),
            _ => None,
        }
    }

    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = match self.current_token {
            Token::BANG => Prefix::NOT,
            Token::MINUS => Prefix::MINUS,
            Token::PLUS => Prefix::PLUS,
            _ => panic!("not support prefix op"),
        };

        self.next_token();
        match self.parse_expr(Precedence::PREFIX) {
            Some(expr) => Some(Expr::Prefix(prefix, Box::new(expr))),
            None => None,
        }
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Option<Expr> {
        let infix = match self.current_token {
            Token::PLUS => Infix::PLUS,
            Token::MINUS => Infix::MINUS,
            Token::SLASH => Infix::DIVIDE,
            Token::ASTERISK => Infix::MULTIPLY,
            Token::EQ => Infix::EQUAL,
            Token::NOT_EQ => Infix::NOTEQUAL,
            Token::LT => Infix::LESSTHAN,
            Token::GT => Infix::GREATERTHAN,
            _ => panic!("do not support {:?}", self.current_token),
        };

        let precedence = self.current_token_precedence();
        self.next_token();
        match self.parse_expr(precedence) {
            Some(expr) => Some(Expr::Infix(infix, Box::new(left), Box::new(expr))),
            None => None,
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.next_token();
        let expr = self.parse_expr(Precedence::LOWEST);
        if !self.expect_next_token(Token::RPAREN) {
            panic!("error");
        } else {
            expr
        }
    }

    fn parse_bool_expr(&mut self) -> Option<Expr> {
        match self.current_token {
            Token::BOOL(value) => Some(Expr::Literal(Literal::Bool(value == true))),
            _ => None,
        }
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        if !self.expect_next_token(Token::LPAREN) {
            panic!(
                "execpt next token {:?} got {:?}",
                Token::LPAREN,
                self.next_token
            );
        }
        self.next_token();
        let cond = match self.parse_expr(Precedence::LOWEST) {
            Some(expr) => expr,
            None => return None,
        };
        if !self.expect_next_token(Token::RPAREN) || !self.expect_next_token(Token::LBRACE) {
            return None;
        }
        let consequence = self.parse_block_stmt();
        let mut alternative = None;

        if self.next_token_is(&Token::ELSE) {
            self.next_token();

            if !self.expect_next_token(Token::LBRACE) {
                return None;
            }

            alternative = Some(self.parse_block_stmt());
        }
        Some(Expr::If {
            cond: Box::new(cond),
            consequence,
            alternative,
        })
    }

    fn parse_block_stmt(&mut self) -> BlockStmt {
        self.next_token();

        let mut block = vec![];

        while !self.current_token_is(Token::RBRACE) && !self.current_token_is(Token::EOF) {
            match self.parse_stmt() {
                Some(stmt) => block.push(stmt),
                None => {}
            }
            self.next_token();
        }
        block
    }

    fn parse_func_expr(&mut self) -> Option<Expr> {
        if !self.expect_next_token(Token::LPAREN) {
            panic!("except {:?} but found {:?}", Token::LPAREN, self.next_token);
        }

        let params = match self.parse_func_params() {
            Some(params) => params,
            None => return None,
        };

        if !self.expect_next_token(Token::LBRACE) {
            panic!("except {:?} but found {:?}", Token::LBRACE, self.next_token);
        }

        Some(Expr::Func {
            params,
            body: self.parse_block_stmt(),
        })
    }

    fn parse_func_params(&mut self) -> Option<Vec<Ident>> {
        let mut params = vec![];
        if self.next_token_is(&Token::RPAREN) {
            self.next_token();
            return Some(params);
        }

        self.next_token();
        match self.parse_ident() {
            Some(ident) => params.push(ident),
            None => return None,
        }

        while self.next_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            match self.parse_ident() {
                Some(ident) => params.push(ident),
                None => return None,
            }
        }
        if !self.expect_next_token(Token::RPAREN) {
            panic!("except {:?} but found {:?}", Token::RPAREN, self.next_token);
        }
        Some(params)
    }

    fn parse_call_expr(&mut self, func: Expr) -> Option<Expr> {
        let args = match self.parse_expr_list(Token::RPAREN) {
            Some(args) => args,
            None => return None,
        };
        Some(Expr::Call {
            func: Box::new(func),
            args,
        })
    }

    fn parse_expr_list(&mut self, end: Token) -> Option<Vec<Expr>> {
        let mut list = vec![];
        if self.next_token_is(&end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        match self.parse_expr(Precedence::LOWEST) {
            Some(expr) => list.push(expr),
            None => return None,
        }

        while self.next_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();

            match self.parse_expr(Precedence::LOWEST) {
                Some(expr) => list.push(expr),
                None => return None,
            }
        }

        if !self.expect_next_token(end) {
            return None;
        }

        Some(list)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_let_stmt() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

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

    #[test]
    fn test_return_stmt() {
        let input = r#"
return 5;
return 10;
return 993322;
        "#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        println!("{:?}", program);

        assert_eq!(
            vec![
                Stmt::Return(Expr::Literal(Literal::Int(5))),
                Stmt::Return(Expr::Literal(Literal::Int(10))),
                Stmt::Return(Expr::Literal(Literal::Int(993322))),
            ],
            program,
        );
    }

    #[test]
    fn test_ident_expr() {
        let input = "foobar;";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        assert_eq!(
            vec![Stmt::Expr(Expr::Ident(Ident(String::from("foobar"))))],
            program,
        );
    }

    #[test]
    fn test_integer_literal_expr() {
        let input = "5;";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        assert_eq!(vec![Stmt::Expr(Expr::Literal(Literal::Int(5)))], program,);
    }

    #[test]
    fn test_infix_expr() {
        let tests = vec![
            (
                "5 + 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::PLUS,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 - 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::MINUS,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 * 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::MULTIPLY,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 / 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::DIVIDE,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 > 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::GREATERTHAN,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 < 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::LESSTHAN,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 == 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::EQUAL,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 != 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::NOTEQUAL,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();
            assert_eq!(vec![expect], program);
        }
    }

    #[test]
    fn test_prefix_expr() {
        let tests = vec![
            (
                "!5;",
                Stmt::Expr(Expr::Prefix(
                    Prefix::NOT,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "-15;",
                Stmt::Expr(Expr::Prefix(
                    Prefix::MINUS,
                    Box::new(Expr::Literal(Literal::Int(15))),
                )),
            ),
            (
                "+15;",
                Stmt::Expr(Expr::Prefix(
                    Prefix::PLUS,
                    Box::new(Expr::Literal(Literal::Int(15))),
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            assert_eq!(vec![expect], program);
        }
    }

    #[test]
    fn test_if_expr() {
        let input = "if (x < y) { x }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        assert_eq!(
            vec![Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::LESSTHAN,
                    Box::new(Expr::Ident(Ident(String::from("x")))),
                    Box::new(Expr::Ident(Ident(String::from("y")))),
                )),
                consequence: vec![Stmt::Expr(Expr::Ident(Ident(String::from("x"))))],
                alternative: None,
            })],
            program,
        );
    }

    #[test]
    fn test_if_else_expr() {
        let input = "if (x < y) { x } else { y }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        assert_eq!(
            vec![Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::LESSTHAN,
                    Box::new(Expr::Ident(Ident(String::from("x")))),
                    Box::new(Expr::Ident(Ident(String::from("y")))),
                )),
                consequence: vec![Stmt::Expr(Expr::Ident(Ident(String::from("x"))))],
                alternative: Some(vec![Stmt::Expr(Expr::Ident(Ident(String::from("y"))))]),
            })],
            program,
        );
    }

    #[test]
    fn test_func_expr() {
        let input = "fn(x, y) { x + y; }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        assert_eq!(
            vec![Stmt::Expr(Expr::Func {
                params: vec![Ident(String::from("x")), Ident(String::from("y"))],
                body: vec![Stmt::Expr(Expr::Infix(
                    Infix::PLUS,
                    Box::new(Expr::Ident(Ident(String::from("x")))),
                    Box::new(Expr::Ident(Ident(String::from("y")))),
                ))],
            })],
            program,
        );
    }

    #[test]
    fn test_func_params() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec![Ident(String::from("x"))]),
            (
                "fn(x, y, z) {};",
                vec![
                    Ident(String::from("x")),
                    Ident(String::from("y")),
                    Ident(String::from("z")),
                ],
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            assert_eq!(
                vec![Stmt::Expr(Expr::Func {
                    params: expect,
                    body: vec![],
                })],
                program,
            );
        }
    }

    #[test]
    fn test_call_expr() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        assert_eq!(
            vec![Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident(Ident(String::from("add")))),
                args: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Infix(
                        Infix::MULTIPLY,
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Box::new(Expr::Literal(Literal::Int(3))),
                    ),
                    Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Literal(Literal::Int(4))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    ),
                ],
            })],
            program,
        );
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            (
                "-a * b",
                Stmt::Expr(Expr::Infix(
                    Infix::MULTIPLY,
                    Box::new(Expr::Prefix(
                        Prefix::MINUS,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("b")))),
                )),
            ),
            (
                "!-a",
                Stmt::Expr(Expr::Prefix(
                    Prefix::NOT,
                    Box::new(Expr::Prefix(
                        Prefix::MINUS,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                    )),
                )),
            ),
            (
                "a + b + c",
                Stmt::Expr(Expr::Infix(
                    Infix::PLUS,
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a + b - c",
                Stmt::Expr(Expr::Infix(
                    Infix::MINUS,
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a * b * c",
                Stmt::Expr(Expr::Infix(
                    Infix::MULTIPLY,
                    Box::new(Expr::Infix(
                        Infix::MULTIPLY,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a * b / c",
                Stmt::Expr(Expr::Infix(
                    Infix::DIVIDE,
                    Box::new(Expr::Infix(
                        Infix::MULTIPLY,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a + b / c",
                Stmt::Expr(Expr::Infix(
                    Infix::PLUS,
                    Box::new(Expr::Ident(Ident(String::from("a")))),
                    Box::new(Expr::Infix(
                        Infix::DIVIDE,
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                        Box::new(Expr::Ident(Ident(String::from("c")))),
                    )),
                )),
            ),
            (
                "a + b * c + d / e - f",
                Stmt::Expr(Expr::Infix(
                    Infix::MINUS,
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Infix(
                            Infix::PLUS,
                            Box::new(Expr::Ident(Ident(String::from("a")))),
                            Box::new(Expr::Infix(
                                Infix::MULTIPLY,
                                Box::new(Expr::Ident(Ident(String::from("b")))),
                                Box::new(Expr::Ident(Ident(String::from("c")))),
                            )),
                        )),
                        Box::new(Expr::Infix(
                            Infix::DIVIDE,
                            Box::new(Expr::Ident(Ident(String::from("d")))),
                            Box::new(Expr::Ident(Ident(String::from("e")))),
                        )),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("f")))),
                )),
            ),
            (
                "(5 + 5) * 2",
                Stmt::Expr(Expr::Infix(
                    Infix::MULTIPLY,
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expr::Literal(Literal::Int(2))),
                )),
            ),
            (
                "2 / (5 + 5)",
                Stmt::Expr(Expr::Infix(
                    Infix::DIVIDE,
                    Box::new(Expr::Literal(Literal::Int(2))),
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "-(5 + 5)",
                Stmt::Expr(Expr::Prefix(
                    Prefix::MINUS,
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "!-a",
                Stmt::Expr(Expr::Prefix(
                    Prefix::NOT,
                    Box::new(Expr::Prefix(
                        Prefix::MINUS,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                    )),
                )),
            ),
            (
                "5 > 4 == 3 < 4",
                Stmt::Expr(Expr::Infix(
                    Infix::EQUAL,
                    Box::new(Expr::Infix(
                        Infix::GREATERTHAN,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::LESSTHAN,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "5 < 4 != 3 > 4",
                Stmt::Expr(Expr::Infix(
                    Infix::NOTEQUAL,
                    Box::new(Expr::Infix(
                        Infix::LESSTHAN,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::GREATERTHAN,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                Stmt::Expr(Expr::Infix(
                    Infix::EQUAL,
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Infix(
                            Infix::MULTIPLY,
                            Box::new(Expr::Literal(Literal::Int(4))),
                            Box::new(Expr::Literal(Literal::Int(5))),
                        )),
                    )),
                    Box::new(Expr::Infix(
                        Infix::PLUS,
                        Box::new(Expr::Infix(
                            Infix::MULTIPLY,
                            Box::new(Expr::Literal(Literal::Int(3))),
                            Box::new(Expr::Literal(Literal::Int(1))),
                        )),
                        Box::new(Expr::Infix(
                            Infix::MULTIPLY,
                            Box::new(Expr::Literal(Literal::Int(4))),
                            Box::new(Expr::Literal(Literal::Int(5))),
                        )),
                    )),
                )),
            ),
            ("true", Stmt::Expr(Expr::Literal(Literal::Bool(true)))),
            ("false", Stmt::Expr(Expr::Literal(Literal::Bool(false)))),
            (
                "3 > 5 == false",
                Stmt::Expr(Expr::Infix(
                    Infix::EQUAL,
                    Box::new(Expr::Infix(
                        Infix::GREATERTHAN,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expr::Literal(Literal::Bool(false))),
                )),
            ),
            (
                "3 < 5 == true",
                Stmt::Expr(Expr::Infix(
                    Infix::EQUAL,
                    Box::new(Expr::Infix(
                        Infix::LESSTHAN,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expr::Literal(Literal::Bool(true))),
                )),
            ),
            (
                "!(true == true)",
                Stmt::Expr(Expr::Prefix(
                    Prefix::NOT,
                    Box::new(Expr::Infix(
                        Infix::EQUAL,
                        Box::new(Expr::Literal(Literal::Bool(true))),
                        Box::new(Expr::Literal(Literal::Bool(true))),
                    )),
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();
            assert_eq!(vec![expect], program);
        }
    }

    
}
