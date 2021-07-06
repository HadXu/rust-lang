use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    next_pos: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            pos: 0,
            next_pos: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }
    #[allow(dead_code)]
    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                b' ' | b'\t' | b'\n' | b'\r' => {
                    self.read_char();
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn read_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.next_pos];
        }
        self.pos = self.next_pos;
        self.next_pos += 1;
    }
    #[allow(dead_code)]
    fn peek_char(&self) -> u8 {
        if self.next_pos >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.next_pos]
        }
    }

    fn nextch_is(&mut self, ch: u8) -> bool {
        self.peek_char() == ch
    }

    #[allow(dead_code)]
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            b'=' => {
                if self.nextch_is(b'=') {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b',' => Token::COMMA,
            b'+' => Token::PLUS,
            b'-' => Token::MINUS,
            b'!' => {
                if self.nextch_is(b'=') {
                    self.read_char();
                    Token::NOT_EQ
                } else {
                    Token::BANG
                }
            }
            b'/' => Token::SLASH,
            b'*' => Token::ASTERISK,
            b'<' => {
                if self.nextch_is(b'=') {
                    self.read_char();
                    Token::LTE
                }else {
                    Token::LT
                }
            }
            b'>' => {
                if self.nextch_is(b'=') {
                    self.read_char();
                    Token::GTE
                }else {
                    Token::GT
                }
            }
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b'[' => Token::LBRACKET,
            b']' => Token::RBRACKET,
            b':' => Token::COLON,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => return self.read_identifier(),
            b'0'..=b'9' => return self.read_number(),
            b'"' => return self.read_string(),
            0 => Token::EOF,
            _ => Token::ILLEGAL,
        };
        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> Token {
        let pos = self.pos;
        loop {
            match self.ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    self.read_char();
                }
                _ => {
                    break;
                }
            }
        }
        let literal = &self.input[pos..self.pos];
        match literal {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "true" => Token::BOOL(true),
            "false" => Token::BOOL(false),
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            "for" => Token::FOR,
            _ => Token::IDENT(String::from(literal)),
        }
    }

    fn read_number(&mut self) -> Token {
        let pos = self.pos;
        loop {
            match self.ch {
                b'0'..=b'9' => {
                    self.read_char();
                }
                _ => {
                    break;
                }
            }
        }

        let literal = &self.input[pos..self.pos];
        Token::INT(literal.parse::<i64>().unwrap())
    }

    fn read_string(&mut self) -> Token {
        self.read_char();
        let start_pos = self.pos;
        loop {
            match self.ch {
                b'"' | 0 => {
                    let literal = &self.input[start_pos..self.pos];
                    self.read_char();
                    return Token::STRING(literal.to_string());
                }
                _ => self.read_char(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_next_token() {
        use crate::lexer::Lexer;
        use crate::token::Token;

        let input = r#"
        let five = 5; 
        let ten = 10;

        let add = fn(x, y)
            {x + y;}; 
        let result = add(five, ten);

        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        }else {
            return false;
        }
        10 == 10;
        10 != 9;
        "foobar";
        "foo bar";
        [1, 2];
        {"foo": "bar"};
        3 <= 5;
        "#;
        let tests = vec![
            Token::LET,
            Token::IDENT(String::from("five")),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("ten")),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("add")),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT(String::from("x")),
            Token::COMMA,
            Token::IDENT(String::from("y")),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT(String::from("x")),
            Token::PLUS,
            Token::IDENT(String::from("y")),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("result")),
            Token::ASSIGN,
            Token::IDENT(String::from("add")),
            Token::LPAREN,
            Token::IDENT(String::from("five")),
            Token::COMMA,
            Token::IDENT(String::from("ten")),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::BOOL(true),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::BOOL(false),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOT_EQ,
            Token::INT(9),
            Token::SEMICOLON,
            Token::STRING(String::from("foobar")),
            Token::SEMICOLON,
            Token::STRING(String::from("foo bar")),
            Token::SEMICOLON,
            Token::LBRACKET,
            Token::INT(1),
            Token::COMMA,
            Token::INT(2),
            Token::RBRACKET,
            Token::SEMICOLON,
            Token::LBRACE,
            Token::STRING(String::from("foo")),
            Token::COLON,
            Token::STRING(String::from("bar")),
            Token::RBRACE,
            Token::SEMICOLON,
            Token::INT(3),
            Token::LTE,
            Token::INT(5),
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);
        for expect in tests {
            let tok = lexer.next_token();
            println!("{:?} {:?}", tok, expect);
            assert_eq!(tok, expect);
        }
    }
}
