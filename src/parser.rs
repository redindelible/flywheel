use std::cmp::Ordering;

#[derive(Debug)]
pub enum TokenType {
    Indent,
    Dedent,
    Integer,
    Ident,
    If,
    Else,
    Return,
    Colon
}

#[derive(Debug)]
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


pub fn lex(mut s: &str) -> Vec<Token> {
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
                    panic!("No matching indent level")
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
                        _ => TokenType::Ident
                    };
                    tokens.push(Token(ty, ident));
                }
                ':' => tokens.push(Token(TokenType::Colon, s.advance_chars(1))),
                ' ' | '\t' | '\r' => {
                    s.advance_chars(1);
                }
                '\n' => {
                    s.advance_chars(1);
                    break;
                }
                c => {
                    println!("Unknown character: {c}");
                    s.advance_chars(1);
                }
            }
        }
    }

    return tokens;
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