use std::borrow::Cow;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt::{Debug, Formatter};
use crate::ast;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TokenType {
    EOF,
    Indent,
    Dedent,
    Newline,
    Integer,
    Ident,
    If,
    Else,
    Def,
    Return,
    Period,
    Comma,
    Plus,
    Minus,
    LeftParen,
    RightParen,
    Colon
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Token<'a>(TokenType, &'a str);


trait StrExt<'a> {
    fn first(&self) -> Option<char>;

    fn advance_chars(&mut self, n: usize) -> &'a str;
    // fn advance_str(&mut self) -> &'a str;
    fn take_while(&mut self, cond: impl FnMut(char) -> bool) -> &'a str;
}

impl<'a> StrExt<'a> for &'a str {
    fn first(&self) -> Option<char> {
        self.chars().next()
    }

    fn advance_chars(&mut self, mut n: usize) -> &'a str {
        n += 1;
        let idx = self.find(|c| {
            n -= 1;
            n == 0
        }).unwrap_or(self.len());
        let (before, after) = self.split_at(idx);
        *self = after;
        before
    }

    fn take_while(&mut self, mut cond: impl FnMut(char) -> bool) -> &'a str {
        let idx = self.find(|char| !cond(char)).unwrap_or(self.len());
        let (before, after) = self.split_at(idx);
        *self = after;
        before
    }
}


pub fn lex(mut s: &str) -> Result<Vec<Token>, String> {
    let mut brackets = 0;
    let mut indent_levels = vec![0];
    let mut tokens: Vec<Token> = vec![];

    while !s.is_empty() {
        let mut line_spaces = 0u32;
        let mut is_comment = false;
        let mut ended_on_newline = false;
        s.take_while(|c| {
            match c {
                ' '  => { line_spaces += 1; true },
                '\t' => { line_spaces += 1; line_spaces = line_spaces.next_multiple_of(8); true }
                '\r' => { true },
                '#' => { is_comment = true; true }
                '\n' => { ended_on_newline = true; false },
                _ => is_comment
            }
        });
        if ended_on_newline {
            s.advance_chars(1);
            continue;
        }
        if s.is_empty() {
            line_spaces = 0;
        }
        match line_spaces.cmp(indent_levels.last().unwrap()) {
            Ordering::Less => {
                if let Some(idx) = indent_levels.iter().rposition(|&level| level == line_spaces) {
                    for _ in indent_levels.drain(idx+1..) {
                        tokens.push(Token(TokenType::Dedent, ""));
                    }
                } else {
                    return Err(String::from("No matching indent level"));
                }
            }
            Ordering::Greater => {
                indent_levels.push(line_spaces);
                tokens.push(Token(TokenType::Indent, ""));
            }
            Ordering::Equal => { }
        }

        while let Some(char) = s.first() {
            match char {
                c if c.is_ascii_digit() => {
                    let number = s.take_while(|c| c.is_ascii_digit());
                    tokens.push(Token(TokenType::Integer, number));
                }
                c if c == '_' || c.is_ascii_alphabetic() => {
                    let ident = s.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
                    let ty = match ident {
                        "if" => TokenType::If,
                        "return" => TokenType::Return,
                        "else" => TokenType::Else,
                        "def" => TokenType::Def,
                        _ => TokenType::Ident
                    };
                    tokens.push(Token(ty, ident));
                }
                ':' => tokens.push(Token(TokenType::Colon, s.advance_chars(1))),
                ',' => tokens.push(Token(TokenType::Comma, s.advance_chars(1))),
                '+' => tokens.push(Token(TokenType::Plus, s.advance_chars(1))),
                '-' => tokens.push(Token(TokenType::Minus, s.advance_chars(1))),
                '(' => {
                    brackets += 1;
                    tokens.push(Token(TokenType::LeftParen, s.advance_chars(1)))
                },
                ')' => {
                    brackets -= 1;
                    tokens.push(Token(TokenType::RightParen, s.advance_chars(1)))
                },
                ' ' | '\t' | '\r' => {
                    s.advance_chars(1);
                }
                '\n' => {
                    if brackets == 0 {
                        tokens.push(Token(TokenType::Newline, s.advance_chars(1)));
                        break;
                    } else {
                        s.advance_chars(1);
                    }
                }
                c => {
                    return Err(format!("Unexpected character: {c}"));
                }
            }
        }
    }

    return Ok(tokens);
}

pub struct ParseError(Cow<'static, str>);

impl From<String> for ParseError {
    fn from(value: String) -> Self {
        ParseError(Cow::from(value))
    }
}

impl From<&'static str> for ParseError {
    fn from(value: &'static str) -> Self {
        ParseError(Cow::from(value))
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse Error: {}", self.0.as_ref())
    }
}


pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    idx: usize
}

impl<'a> Parser<'a> {
    pub fn from_text(text: &'a str) -> Result<Parser<'a>, String> {
        Ok(Parser::from_tokens(lex(text)?))
    }

    pub fn from_tokens(tokens: Vec<Token<'a>>) -> Parser<'a> {
        Parser { tokens, idx: 0 }
    }

    fn curr(&self) -> Token<'a> {
        self.tokens.get(self.idx).copied().unwrap_or(Token(TokenType::EOF, ""))
    }

    fn advance(&mut self) -> Token<'a> {
        let ret = self.curr();
        self.idx += 1;
        ret
    }

    fn expect(&mut self, ty: TokenType) -> ParseResult<Token<'a>> {
        let curr = self.curr();
        if curr.0 == ty {
            self.advance();
            Ok(curr)
        } else {
            Err(format!("Expected a {ty:?} token").into())
        }
    }

    pub fn parse_stmt(&mut self) -> ParseResult<ast::Stmt> {
        match self.curr() {
            Token(TokenType::If, _) => {
                self.advance();
                let cond = self.parse_expr()?;
                self.expect(TokenType::Newline)?; self.expect(TokenType::Indent)?;
                let mut then_stmts = Vec::new();
                while self.curr().0 != TokenType::Dedent {
                    let stmt = self.parse_stmt()?;
                    self.expect(TokenType::Newline)?;
                    then_stmts.push(stmt);
                }
                self.expect(TokenType::Dedent)?; self.expect(TokenType::Else)?;

                self.expect(TokenType::Newline)?; self.expect(TokenType::Indent)?;
                let mut else_stmts = Vec::new();
                while self.curr().0 != TokenType::Dedent {
                    let stmt = self.parse_stmt()?;
                    self.expect(TokenType::Newline)?;
                    else_stmts.push(stmt);
                }
                self.expect(TokenType::Dedent)?;

                Ok(ast::Stmt::If(Box::new(cond), then_stmts, else_stmts))
            }
            Token(TokenType::Return, _) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenType::Newline)?;

                Ok(ast::Stmt::Return(Box::new(expr)))
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenType::Newline)?;

                Ok(ast::Stmt::Expr(Box::new(expr)))
            }
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<ast::Expr> {
        self.parse_expr_add()
    }

    fn parse_expr_add(&mut self) -> ParseResult<ast::Expr> {
        let mut left = self.parse_expr_mul()?;
        loop {
            match self.curr() {
                Token(TokenType::Plus, _) => {
                    self.advance();
                    left = ast::Expr::Binary(ast::BinaryOp::Add, Box::new(left), Box::new(self.parse_expr_mul()?));
                }
                Token(TokenType::Minus, _) => {
                    self.advance();
                    left = ast::Expr::Binary(ast::BinaryOp::Sub, Box::new(left), Box::new(self.parse_expr_mul()?));
                }
                _ => break
            }
        }
        Ok(left)
    }

    fn parse_expr_mul(&mut self) -> ParseResult<ast::Expr> {
        let left = self.parse_expr_call()?;
        loop {
            match self {
                _ => break
            }
        }
        Ok(left)
    }

    fn parse_expr_call(&mut self) -> ParseResult<ast::Expr> {
        let mut left = self.parse_expr_terminal()?;
        loop {
            match self.curr() {
                Token(TokenType::Period, _) => {
                    self.advance();
                    let attr = self.expect(TokenType::Ident)?.1;
                    left = ast::Expr::Attr(Box::new(left), Box::from(attr));
                }
                Token(TokenType::LeftParen, _) => {
                    self.advance();
                    let mut arguments = Vec::new();
                    while self.curr().0 != TokenType::RightParen {
                        arguments.push(self.parse_expr()?);
                        if self.curr().0 == TokenType::Comma {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenType::RightParen)?;

                    left = ast::Expr::Call(Box::new(left), arguments.into_boxed_slice());
                }
                _ => break
            }
        }
        Ok(left)
    }

    fn parse_expr_terminal(&mut self) -> ParseResult<ast::Expr> {
        match self.curr() {
            Token(TokenType::Ident, name) => {
                self.advance();
                Ok(ast::Expr::Name(Box::from(name)))
            }
            Token(TokenType::Integer, num) => {
                self.advance();
                let num = num.parse::<i64>().map_err(|err| ParseError(err.to_string().into()))?;
                Ok(ast::Expr::Integer(num))
            }
            // Token(TokenType::, num) => {
            //     self.advance();
            //     let num = num.parse::<i64>().map_err(|err| err.to_string().into())?;
            //     Ok(ast::Expr::Integer(num))
            // }
            // Token(TokenType::Integer, num) => {
            //     self.advance();
            //     let num = num.parse::<i64>().map_err(|err| err.to_string().into())?;
            //     Ok(ast::Expr::Integer(num))
            // }
            Token(other, _) => {
                Err(ParseError::from(format!("Unexpected token: {other:?}")))
            }
        }
    }
}


#[cfg(test)]
mod test {
    use crate::parser::{lex, StrExt};

    #[test]
    fn test_lex() {
        let tokens = lex(r"
if b:
    return 4
else:
    return val
        ");
        dbg!(tokens);
    }

    #[test]
    fn test_take_while() {
        let mut s = "124 436";
        let a = s.take_while(|c| c.is_ascii_digit());
        let b = s.take_while(|c| c.is_ascii_whitespace());
        let c = s.take_while(|c| c.is_ascii_digit());
        assert_eq!(a, "124");
        assert_eq!(b, " ");
        assert_eq!(c, "436");
        assert_eq!(s, "");
    }

    #[test]
    fn test_advance_char() {
        let mut s = "124 436";
        let a = s.advance_chars(4);
        let b = s.advance_chars(4);
        assert_eq!(a, "124 ");
        assert_eq!(b, "436");
        assert_eq!(s, "");
    }
}