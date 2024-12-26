use std::collections::HashSet;
use std::sync::Arc;
use crate::frontend::{ast::{self, AST, ASTBuilder, AstRef}, lexer::Lexer, source::Location, token::{Token, TokenType}, StringsTable};
use crate::frontend::source::Source;
use crate::frontend::token::TokenStream;

pub fn parse(context: Arc<StringsTable>, source_arc: Arc<Source>) -> ParseResult<Arc<AST>> {
    let ast = AST::new(context, |context, builder| {
        let lexer = Lexer::new(context, source_arc);
        Parser::new(lexer, builder).parse_file()
    })?;
    Ok(Arc::new(ast))
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Located(String, Location),
    IntegerOutOfRange(String, Location)
}

impl ParseError {
    fn integer_too_big(location: Location) -> Self {
        ParseError::IntegerOutOfRange(String::from("Error: Integer literal too large."), location)
    }
    
    fn expected_any_of(possible: &[TokenType], actual: Token) -> Self {
        if possible.len() == 0 {
            unreachable!()
        } else if possible.len() == 1 {
            let s = format!("Error: Got {}, expected {}.", actual.ty.name(), possible[0].name());
            ParseError::Located(s, actual.loc)
        } else if possible.len() == 2 {
            let s = format!("Error: Got {}, expected {} or {}.", actual.ty.name(), possible[0].name(), possible[1].name());
            ParseError::Located(s, actual.loc)
        } else {
            todo!()
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

struct Parser<'ast, L> {
    token_stream: L,
    curr: Option<Token>,
    
    ast: &'ast mut ASTBuilder,

    possible_tokens: HashSet<TokenType>
}

impl<'ast, L: TokenStream> Parser<'ast, L> {
    fn new(mut lexer: L, ast: &'ast mut ASTBuilder) -> Self {
        let curr = lexer.next();
        Parser { token_stream: lexer, curr, ast, possible_tokens: HashSet::new() }
    }

    fn advance(&mut self) -> Token {
        if let Some(token) = self.curr {
            self.possible_tokens.clear();
            self.curr = self.token_stream.next();
            token
        } else {
            Token::new_eof(self.token_stream.source_id())
        }
    }

    fn curr_is_ty(&mut self, ty: TokenType) -> bool {
        self.possible_tokens.insert(ty);
        let curr_ty = self.curr.map_or(TokenType::EOF, |token| token.ty);
        curr_ty == ty
    }

    fn error_expected_none<T>(&mut self) -> ParseResult<T> {
        let curr = self.curr.unwrap_or(Token::new_eof(self.token_stream.source_id()));
        let tys: Vec<TokenType> = self.possible_tokens.drain().collect();
        Err(ParseError::expected_any_of(&tys, curr))
    }

    fn expect(&mut self, ty: TokenType) -> ParseResult<Token> {
        self.possible_tokens.insert(ty);
        let curr = self.curr.unwrap_or(Token::new_eof(self.token_stream.source_id()));
        if curr.ty == ty {
            self.advance();
            Ok(curr)
        } else {
            self.error_expected_none()
        }
    }
    
    fn parse_file(&mut self) -> ParseResult<AstRef<ast::File>> {
        let mut top_levels = vec![];
        while !self.curr_is_ty(TokenType::EOF) {
            top_levels.push(self.parse_top_level()?);
        }
        let top_levels = self.ast.new_list(top_levels);
        let file = self.ast.new_node(ast::File { top_levels }, Location::new_file(self.token_stream.source_id()));
        Ok(file)
    }
    
    fn parse_top_level(&mut self) -> ParseResult<ast::TopLevel> {
        if self.curr_is_ty(TokenType::Fn) {
            Ok(ast::TopLevel::Function(self.parse_function()?))
        } else {
            self.error_expected_none()
        }
    }
    
    fn parse_function(&mut self) -> ParseResult<AstRef<ast::Function>> {
        let start = self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Identifier)?.text.unwrap();
        self.expect(TokenType::LeftParenthesis)?;
        self.expect(TokenType::RightParenthesis)?;
        self.expect(TokenType::LeftArrow)?;
        let return_type = self.parse_type()?;
        let body = self.parse_block()?;
        let loc = start.loc.combine(self.ast.get_location(return_type));
        Ok(self.ast.new_node(ast::Function { name, return_type, body }, loc))
    }

    fn parse_block(&mut self) -> ParseResult<AstRef<ast::Block>> {
        let start = self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        let mut trailing_expr = None;
        while !self.curr_is_ty(TokenType::RightBrace) {
            let maybe_stmt = self.parse_stmt()?;
            if let Some(stmt) = maybe_stmt {
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr()?;
                if self.curr_is_ty(TokenType::Semicolon) {
                    let end = self.advance();
                    let stmt = self.ast.new_node(ast::Stmt::Expr(expr), self.ast.get_location(expr).combine(end.loc));
                    stmts.push(stmt);
                } else {
                    trailing_expr = Some(expr);
                    break;
                }
            }
        }
        let end = self.expect(TokenType::RightBrace)?;
        let block = ast::Block { stmts: self.ast.new_list(stmts), trailing_expr };
        Ok(self.ast.new_node(block, start.loc.combine(end.loc)))
    }

    fn parse_stmt(&mut self) -> ParseResult<Option<AstRef<ast::Stmt>>> {
        if self.curr_is_ty(TokenType::Return) {
            let start = self.expect(TokenType::Return)?;
            let expr = self.parse_expr()?;
            let end = self.expect(TokenType::Semicolon)?;
            Ok(Some(self.ast.new_node(ast::Stmt::Return(expr), start.loc.combine(end.loc))))
        } else {
            Ok(None)
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<AstRef<ast::Expr>> {
        self.parse_expr_add()
    }

    fn parse_expr_add(&mut self) -> ParseResult<AstRef<ast::Expr>> {
        let mut left = self.parse_expr_mul()?;
        loop {
            let op = if self.curr_is_ty(TokenType::Plus) {
                ast::BinaryOp::Add
            } else if self.curr_is_ty(TokenType::Minus) {
                ast::BinaryOp::Sub
            } else {
                break
            };

            self.advance();
            let right = self.parse_expr()?;
            left = self.ast.new_node(
                ast::Expr::Binary { op, left, right },
                self.ast.get_location(left).combine(self.ast.get_location(right))
            )
        }
        Ok(left)
    }

    fn parse_expr_mul(&mut self) -> ParseResult<AstRef<ast::Expr>> {
        let left = self.parse_expr_call()?;
        loop {
            break
        }
        Ok(left)
    }

    fn parse_expr_call(&mut self) -> ParseResult<AstRef<ast::Expr>> {
        let mut left = self.parse_expr_terminal()?;
        loop {
            if self.curr_is_ty(TokenType::Period) {
                self.advance();
                let attr = self.expect(TokenType::Identifier)?;
                left = self.ast.new_node(
                    ast::Expr::Attr {
                        object: left,
                        name: attr.text.unwrap()
                    },
                    self.ast.get_location(left).combine(attr.loc)
                );
            } else if self.curr_is_ty(TokenType::LeftParenthesis) {
                self.advance();
                let mut arguments = Vec::new();
                while !self.curr_is_ty(TokenType::RightParenthesis) {
                    arguments.push(self.parse_expr()?);
                    if self.curr_is_ty(TokenType::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let end = self.expect(TokenType::RightParenthesis)?;

                let arguments = self.ast.new_list(arguments);
                left = self.ast.new_node(
                    ast::Expr::Call {
                        callee: left,
                        arguments
                    },
                    self.ast.get_location(left).combine(end.loc)
                );
            } else {
                break
            }
        }
        Ok(left)
    }

    fn parse_expr_terminal(&mut self) -> ParseResult<AstRef<ast::Expr>> {
        if self.curr_is_ty(TokenType::Identifier) {
            let name = self.expect(TokenType::Identifier)?;
            Ok(self.ast.new_node(ast::Expr::Name(name.text.unwrap()), name.loc))
        } else if self.curr_is_ty(TokenType::Integer) {
            let constant = self.expect(TokenType::Integer)?;
            let number = self.token_stream.strings().resolve(constant.text.unwrap()).unwrap().parse::<i64>();
            match number {
                Ok(number) => Ok(self.ast.new_node(ast::Expr::Integer(number), constant.loc)),
                Err(_) => Err(ParseError::integer_too_big(constant.loc)),
            }
        } else {
            self.error_expected_none()
        }
    }

    fn parse_type(&mut self) -> ParseResult<AstRef<ast::Type>> {
        let token = self.expect(TokenType::Identifier)?;
        Ok(self.ast.new_node(ast::Type::Name(token.text.unwrap()), token.loc))
    }
}
