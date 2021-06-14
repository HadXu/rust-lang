pub mod object;
use crate::ast::*;
use crate::evaluator::object::*;

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {}
    }

    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result = None;
        for stmt in program {
            match self.eval_stmt(stmt) {
                obj => result = obj,
            }
        }
        result
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Option<Object> {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(expr),
            _ => None,
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Option<Object> {
        match expr {
            Expr::Literal(literal) => Some(self.eval_literal(literal)),
            Expr::Prefix(prefix, right_expr) => {
                let right = self.eval_expr(*right_expr);
                if right.is_some() {
                    Some(self.eval_prefix_expr(prefix, right.unwrap()))
                } else {
                    None
                }
            }

            Expr::Infix(infix, left_expr, right_expr) => {
                let left = self.eval_expr(*left_expr);
                let right = self.eval_expr(*right_expr);
                if left.is_some() && right.is_some() {
                    Some(self.eval_infix_expr(infix, left.unwrap(), right.unwrap()))
                } else {
                    None
                }
            }
            _ => panic!("error"),
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Int(value) => Object::Int(value),
            Literal::Bool(flag) => Object::Bool(flag),
        }
    }

    fn eval_infix_expr(&mut self, infix: Infix, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Int(left_value), Object::Int(right_value)) => {
                self.eval_infix_int_expr(infix, left_value, right_value)
            },
            (_, _) => panic!("not support"),
        }
    }

    fn eval_prefix_expr(&mut self, prefix: Prefix, right: Object) -> Object {
        match right {
            Object::Int(right_value) => self.eval_prefix_int_expr(prefix, right_value),
            Object::Bool(right_value) => self.eval_prefix_bool_expr(prefix, right_value),
        }
    }

    fn eval_infix_int_expr(&mut self, infix: Infix, left: i64, right: i64) -> Object {
        match infix {
            Infix::PLUS => Object::Int(left + right),
            Infix::MINUS => Object::Int(left - right),
            Infix::MULTIPLY => Object::Int(left * right),
            Infix::DIVIDE => Object::Int(left / right),
            Infix::LESSTHAN => Object::Bool(left < right),
            Infix::GREATERTHAN => Object::Bool(left > right),
            Infix::EQUAL => Object::Bool(left == right),
            Infix::NOTEQUAL => Object::Bool(left != right),
        }
    }

    fn eval_prefix_int_expr(&mut self, prefix: Prefix, right: i64) -> Object {
        match prefix {
            Prefix::MINUS => Object::Int(-right),
            Prefix::PLUS => Object::Int(right),
            Prefix::NOT => Object::Int(!right),
        }
    }

    fn eval_prefix_bool_expr(&mut self, prefix: Prefix, right: bool) -> Object {
        match prefix {
            Prefix::NOT => Object::Bool(!right),
            _ => panic!("not support this op {:?}", prefix),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::object::*;
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval(input: &str) -> Option<Object> {
        Evaluator::new().eval(Parser::new(Lexer::new(input)).parse())
    }

    #[test]
    fn test_int_expr() {
        let tests = vec![
            ("5", Some(Object::Int(5))),
            ("10", Some(Object::Int(10))),
            ("-5", Some(Object::Int(-5))),
            ("-10", Some(Object::Int(-10))),
            ("+5", Some(Object::Int(5))),
            ("+10", Some(Object::Int(10))),
            ("+(-5)", Some(Object::Int(-5))),
            ("+(-10)", Some(Object::Int(-10))),
            ("5 + 5 + 5 + 5 - 10", Some(Object::Int(10))),
            ("2 * 2 * 2 * 2 * 2", Some(Object::Int(32))),
            ("-50 + 100 + -50", Some(Object::Int(0))),
            ("5 * 2 + 10", Some(Object::Int(20))),
            ("5 + 2 * 10", Some(Object::Int(25))),
            ("20 + 2 * -10", Some(Object::Int(0))),
            ("50 / 2 * 2 + 10", Some(Object::Int(60))),
            ("2 * (5 + 10)", Some(Object::Int(30))),
            ("3 * 3 * 3 + 10", Some(Object::Int(37))),
            ("3 * (3 * 3) + 10", Some(Object::Int(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Some(Object::Int(50))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_boolean_expr() {
        let tests = vec![
            ("true", Some(Object::Bool(true))),
            ("false", Some(Object::Bool(false))),
            ("1 < 2", Some(Object::Bool(true))),
            ("1 > 2", Some(Object::Bool(false))),
            ("1 < 1", Some(Object::Bool(false))),
            ("1 > 1", Some(Object::Bool(false))),
            ("1 == 1", Some(Object::Bool(true))),
            ("1 != 1", Some(Object::Bool(false))),
            ("1 == 2", Some(Object::Bool(false))),
            ("1 != 2", Some(Object::Bool(true))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
