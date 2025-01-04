use std::collections::HashSet;
use std::sync::Arc;
use crate::frontend::{ast::{self, FileAST, ASTBuilder, AstRef}, lexer::Lexer, source::Location, token::{Token, TokenType}, CompileResult, StringsTable};
use crate::frontend::ast::AstListRef;
use crate::frontend::error::CompileError;
use crate::frontend::source::Source;
use crate::frontend::token::TokenStream;

pub fn parse(context: Arc<StringsTable>, source: &Source) -> CompileResult<Arc<FileAST>> {
    let ast = FileAST::new(context, |context, builder| {
        let lexer = Lexer::new(context, source.id(), source.text());
        let mut parser = Parser::new(lexer, builder);
        match parser.parse_file() {
            Ok(top_levels) => Ok(top_levels),
            Err(_) => Err(parser.error.unwrap())
        }
    })?;
    Ok(Arc::new(ast))
}

fn error_integer_too_big(location: Location) -> CompileError {
    CompileError::with_description_and_location("parse/integer-too-big", "Integer literal too large.", location)
}

fn error_expected_any_of(possible: &[TokenType], actual: Token) -> CompileError {
    let description = match possible {
        [] => unreachable!(),
        [item] => {
            format!("Got {}, expected {}.", actual.ty.name(), item.name())
        }
        [item_a, item_b] => {
            format!("Got {}, expected {} or {}.", actual.ty.name(), item_a.name(), item_b.name())
        }
        [items @ .., last] => {
            format!("Got {}, expected any of {}, or {}.", actual.ty.name(), items.iter().map(TokenType::name).collect::<Vec<_>>().join(", "), last.name())
        }
    };
    CompileError::with_description_and_location("parse/expected-any-of", description, actual.loc)
}

type ParseResult<T> = Result<T, usize>;

struct Parser<'ast, L> {
    token_stream: L,
    last: Option<Token>,
    curr: Option<Token>,
    
    ast: &'ast mut ASTBuilder,

    possible_tokens: HashSet<TokenType>,
    error: Option<CompileError>
}

impl<'ast, L: TokenStream> Parser<'ast, L> {
    fn new(mut lexer: L, ast: &'ast mut ASTBuilder) -> Self {
        let curr = lexer.next();
        Parser { token_stream: lexer, last: None, curr, ast, possible_tokens: HashSet::new(), error: None }
    }

    fn advance(&mut self) -> Token {
        if let Some(token) = self.curr {
            self.possible_tokens.clear();
            (self.last, self.curr) = (self.curr, self.token_stream.next());
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
    
    fn last_was_ty(&mut self, ty: TokenType) -> bool {
        self.last.is_some_and(|token| token.ty == ty)
    }
    
    fn error<T>(&mut self, error: CompileError) -> ParseResult<T> {
        self.error = Some(error);
        Err(0)
    }

    fn error_expected_none<T>(&mut self) -> ParseResult<T> {
        let curr = self.curr.unwrap_or(Token::new_eof(self.token_stream.source_id()));
        let tys: Vec<TokenType> = self.possible_tokens.drain().collect();
        self.error(error_expected_any_of(&tys, curr))
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
    
    fn parse_file(&mut self) -> ParseResult<AstListRef<ast::TopLevel>> {
        let mut top_levels = vec![];
        while !self.curr_is_ty(TokenType::EOF) {
            top_levels.push(self.parse_top_level()?);
        }
        let top_levels = self.ast.new_list(top_levels);
        Ok(top_levels)
    }
    
    fn parse_top_level(&mut self) -> ParseResult<ast::TopLevel> {
        if self.curr_is_ty(TokenType::Struct) {
            Ok(ast::TopLevel::Struct(self.parse_struct()?))
        } else if self.curr_is_ty(TokenType::Fn) {
            Ok(ast::TopLevel::Function(self.parse_function()?))
        } else if self.curr_is_ty(TokenType::Import) {
            Ok(ast::TopLevel::Import(self.parse_import()?))
        } else {
            self.error_expected_none()
        }
    }
    
    fn parse_import(&mut self) -> ParseResult<AstRef<ast::Import>> {
        let start = self.expect(TokenType::Import)?;
        let path = self.expect(TokenType::String)?.text.unwrap();
        let end = self.expect(TokenType::Semicolon)?;
        Ok(self.ast.new_node(ast::Import { relative_path: path }, start.loc.combine(end.loc)))
    }

    fn parse_struct(&mut self) -> ParseResult<AstRef<ast::Struct>> {
        let start = self.expect(TokenType::Struct)?;
        let name = self.expect(TokenType::Identifier)?.text.unwrap();

        let mut fields = Vec::new();
        self.expect(TokenType::LeftBrace)?;
        while !self.curr_is_ty(TokenType::RightBrace) {
            fields.push(self.parse_field()?);
        }
        let end = self.expect(TokenType::RightBrace)?;
        let fields = self.ast.new_list(fields);
        Ok(self.ast.new_node(ast::Struct { name, fields }, start.loc.combine(end.loc)))
    }

    fn parse_field(&mut self) -> ParseResult<AstRef<ast::StructField>> {
        let name = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let ty = self.parse_type()?;
        let end = self.expect(TokenType::Semicolon)?;
        Ok(self.ast.new_node(ast::StructField { name: name.text.unwrap(), ty }, name.loc.combine(end.loc)))
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
            if let Some(expr) = trailing_expr.take() {
                let location = if self.last_was_ty(TokenType::RightBrace) {
                    self.ast.get_location(expr)
                } else {
                    let end = self.expect(TokenType::Semicolon)?;
                    self.ast.get_location(expr).combine(end.loc)
                };
                let stmt = self.ast.new_node(ast::Stmt::Expr(expr), location);
                stmts.push(stmt);
            }
            
            let maybe_stmt = self.parse_stmt()?;
            if let Some(stmt) = maybe_stmt {
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr()?;
                trailing_expr = Some(expr);
            }
        }
        let end = self.expect(TokenType::RightBrace)?;
        let block = ast::Block { stmts: self.ast.new_list(stmts), trailing_expr };
        Ok(self.ast.new_node(block, start.loc.combine(end.loc)))
    }

    fn parse_stmt(&mut self) -> ParseResult<Option<AstRef<ast::Stmt>>> {
        if self.curr_is_ty(TokenType::Let) {
            let start = self.expect(TokenType::Let)?;
            let name = self.expect(TokenType::Identifier)?.text.unwrap();
            let ty = if self.curr_is_ty(TokenType::Colon) {
                self.expect(TokenType::Colon)?;
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenType::Equal)?;
            let value = self.parse_expr()?;
            let end = self.expect(TokenType::Semicolon)?;
            Ok(Some(self.ast.new_node(ast::Stmt::Let { name, ty, value }, start.loc.combine(end.loc))))
        } else if self.curr_is_ty(TokenType::While) {
            let start = self.expect(TokenType::While)?;
            let condition = self.parse_expr()?;
            let body = self.parse_block()?;
            let end = if self.curr_is_ty(TokenType::Semicolon) {
                self.advance().loc
            } else {
                self.ast.get_location(body)
            };
            Ok(Some(self.ast.new_node(ast::Stmt::While { condition, body }, start.loc.combine(end))))
        } else if self.curr_is_ty(TokenType::Return) {
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
                Err(_) => self.error(error_integer_too_big(constant.loc)),
            }
        } else if self.curr_is_ty(TokenType::LeftBrace) {
            let block = self.parse_block()?;
            Ok(self.ast.new_node(ast::Expr::Block(block), self.ast.get_location(block)))
        } else if self.curr_is_ty(TokenType::LeftParenthesis) {
            self.advance();
            let expr = self.parse_expr()?;
            self.expect(TokenType::RightParenthesis)?;
            Ok(expr)
        } else if self.curr_is_ty(TokenType::If) {
            let start = self.advance();
            let condition = if self.curr_is_ty(TokenType::LeftParenthesis) {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenType::RightParenthesis)?;
                expr
            } else {
                self.parse_expr()?
            };
            let then_do = self.parse_expr()?;
            let (else_do, end) = if self.curr_is_ty(TokenType::Else) {
                self.advance();
                let expr = self.parse_expr()?;
                (Some(expr), self.ast.get_location(expr))
            } else {
                (None, self.ast.get_location(then_do))
            };
            Ok(self.ast.new_node(ast::Expr::IfElse { condition, then_do, else_do }, start.loc.combine(end)))
        } else {
            self.error_expected_none()
        }
    }

    fn parse_type(&mut self) -> ParseResult<AstRef<ast::Type>> {
        let token = self.expect(TokenType::Identifier)?;
        Ok(self.ast.new_node(ast::Type::Name(token.text.unwrap()), token.loc))
    }
}


#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use crate::frontend::FrontendDriver;

    fn render_ast(text: String, name: String) -> String {
        let driver = FrontendDriver::new();
        let source = driver.add_string_source(name, text);
        let ast = driver.block_on(driver.query_ast(source)).unwrap();
        let pretty = ast.pretty(2);
        pretty
    }
    
    macro_rules! run_ast_test {
        ($s:literal) => {{
            let source = include_str!(concat!("../../test/", $s));
            let expected = include_str!(concat!("../../test/", $s, ".ast"));
            let pretty = render_ast(source.into(), $s.into());
            assert_eq!(pretty, expected, "(Parsed AST) == (Expected AST)");
        }};
    }

    #[test]
    fn test_simple() {
        run_ast_test!("simple.fly");
    }

    #[test]
    fn test_simple_return() {
        run_ast_test!("simple-return.fly");
    }

    #[test]
    fn test_simple_struct() {
        run_ast_test!("simple-struct.fly");
    }

    #[test]
    fn test_control_flow() {
        run_ast_test!("control-flow.fly");
    }

    #[test]
    fn test_import() {
        run_ast_test!("import.fly");
    }
}
