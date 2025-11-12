use std::collections::HashSet;
use std::marker::PhantomData;

use bumpalo::Bump;

use flywheel_sources::{SourceMap, SourceId, Span};

use crate::ast::{self, FileAST};
use crate::driver::Handle;
use crate::error::{CompileError, CompileResult};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

// fn error_integer_too_big(location: Location) -> CompileError {
//     CompileError::with_description_and_location("parse/integer-too-big", "Integer literal too large.", location)
// }

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
            format!(
                "Got {}, expected any of {}, or {}.",
                actual.ty.name(),
                items.iter().map(TokenType::name).collect::<Vec<_>>().join(", "),
                last.name()
            )
        }
    };
    CompileError::with_description_and_location("parse/expected-any-of", description, actual.loc)
}

type ParseResult<T> = Result<T, usize>;

#[derive(Copy, Clone)]
struct Start(usize);

struct Parser<'ast> {
    tokens: Lexer<'ast>,
    curr: Token,
    last_ty: TokenType,
    last_end: usize,

    ast_arena: &'ast Bump,

    possible_tokens: HashSet<TokenType>,
    error: Option<CompileError>,
}

impl<'ast> Parser<'ast> {
    fn new(mut lexer: Lexer<'ast>, ast_arena: &'ast Bump) -> Self {
        Parser { curr: lexer.eof(), last_ty: TokenType::Eof, last_end: 0, tokens: lexer, ast_arena, possible_tokens: HashSet::new(), error: None }
    }

    fn advance(&mut self) -> Token {
        self.possible_tokens.clear();
        self.last_ty = self.curr.ty;
        self.last_end = self.tokens.position();
        self.curr = self.tokens.advance();
        self.curr
    }

    fn start(&self) -> Start {
        Start(self.tokens.position())
    }

    fn span_from(&self, point: Start) -> Span {
        self.tokens.source().span(point.0..self.last_end)
    }

    fn curr_is_ty(&mut self, ty: TokenType) -> bool {
        self.possible_tokens.insert(ty);
        self.curr.ty == ty
    }

    fn last_was_ty(&mut self, ty: TokenType) -> bool {
        self.last_ty == ty
    }

    fn error<T>(&mut self, error: CompileError) -> ParseResult<T> {
        self.error = Some(error);
        Err(0)
    }

    fn error_expected_none<T>(&mut self) -> ParseResult<T> {
        let tys: Vec<TokenType> = self.possible_tokens.drain().collect();
        self.error(error_expected_any_of(&tys, self.curr))
    }

    fn expect(&mut self, ty: TokenType) -> ParseResult<Token> {
        if self.curr_is_ty(ty) {
            self.advance();
            Ok(self.curr)
        } else {
            self.error_expected_none()
        }
    }

    // todo move all the allocs to alloc_try_with
    //   should mean the compiler won't keep all the ast structs around
    //   because it got confused by allocation

    // todo these should all be global functions since they need to use the above apis
    fn parse_file(&mut self) -> ParseResult<&'ast [ast::TopLevel<'ast>]> {
        let mut top_levels = vec![];
        while !self.curr_is_ty(TokenType::Eof) {
            top_levels.push(self.parse_top_level()?);
        }
        let top_levels = self.ast_arena.new_list(top_levels);
        Ok(top_levels)
    }

    fn parse_top_level(&mut self) -> ParseResult<ast::TopLevel<'ast>> {
        if self.curr_is_ty(TokenType::Struct) {
            Ok(ast::TopLevel::Struct(self.ast_arena.alloc(self.parse_struct()?)))
        } else if self.curr_is_ty(TokenType::Fn) {
            Ok(ast::TopLevel::Function(self.ast_arena.alloc(self.parse_function()?)))
        } else if self.curr_is_ty(TokenType::Import) {
            Ok(ast::TopLevel::Import(self.ast_arena.alloc(self.parse_import()?)))
        } else {
            self.error_expected_none()
        }
    }

    fn parse_import(&mut self) -> ParseResult<ast::Import<'ast>> {
        let start = self.start();
        self.expect(TokenType::Import)?;
        let path = self.expect(TokenType::String)?.span;
        self.expect(TokenType::Semicolon)?;
        Ok(ast::Import { relative_path: path, span: self.span_from(start) })
    }

    fn parse_struct(&mut self) -> ParseResult<ast::Struct<'ast>> {
        let start = self.start();
        self.expect(TokenType::Struct)?;
        let name = self.expect(TokenType::Identifier)?.span;

        let mut fields = Vec::new();
        self.expect(TokenType::LeftBrace)?;
        while !self.curr_is_ty(TokenType::RightBrace) {
            fields.push(self.parse_field()?);
        }
        self.expect(TokenType::RightBrace)?;
        Ok(ast::Struct { name, fields: self.ast_arena.alloc_slice_fill_iter(fields), span: self.span_from(start) })
    }

    fn parse_field(&mut self) -> ParseResult<ast::StructField<'ast>> {
        let start = self.start();
        let name = self.expect(TokenType::Identifier)?.span;
        self.expect(TokenType::Colon)?;
        let ty = self.parse_type()?;
        self.expect(TokenType::Semicolon)?;
        Ok(ast::StructField { name, ty, span: self.span_from(start) })
    }

    fn parse_function(&mut self) -> ParseResult<ast::Function<'ast>> {
        let start = self.start();
        self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Identifier)?.span;
        self.expect(TokenType::LeftParenthesis)?;
        self.expect(TokenType::RightParenthesis)?;
        self.expect(TokenType::LeftArrow)?;
        let return_type = self.parse_type()?;
        let body = self.parse_block()?;
        Ok(ast::Function { name, return_type, body, span: self.span_from(start) })
    }

    fn parse_block(&mut self) -> ParseResult<ast::Block<'ast>> {
        let start = self.start();
        self.expect(TokenType::LeftBrace)?;
        let mut statements = Vec::new();
        let mut trailing_expr: Option<ast::Expr<'ast>> = None;
        while !self.curr_is_ty(TokenType::RightBrace) {
            if let Some(expr) = trailing_expr.take() {
                if !self.last_was_ty(TokenType::RightBrace) {
                    self.expect(TokenType::Semicolon)?;
                }
                // todo include semicolon in span
                statements.push(ast::Stmt::Expr(self.ast_arena.alloc(expr)));
            }

            let maybe_stmt = self.parse_stmt()?;
            if let Some(stmt) = maybe_stmt {
                statements.push(stmt);
            } else {
                trailing_expr = Some(self.parse_expr()?);
            }
        }
        self.expect(TokenType::RightBrace)?;
        let block = ast::Block { stmts: self.ast_arena.alloc_slice_fill_iter(statements), trailing_expr, span: self.span_from(start) };
        Ok(block)
    }

    fn parse_stmt(&mut self) -> ParseResult<Option<ast::Stmt<'ast>>> {
        let start = self.start();
        if self.curr_is_ty(TokenType::Let) {
            self.expect(TokenType::Let)?;
            let name = self.expect(TokenType::Identifier)?.span;
            let ty = if self.curr_is_ty(TokenType::Colon) {
                self.expect(TokenType::Colon)?;
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenType::Equal)?;
            let value = self.parse_expr()?;
            self.expect(TokenType::Semicolon)?;
            Ok(Some(ast::Stmt::Let(self.ast_arena.alloc(ast::Let { name, ty, value, span: self.span_from(start) }))))
        } else if self.curr_is_ty(TokenType::While) {
            self.expect(TokenType::While)?;
            let condition = self.parse_expr()?;
            let body = self.parse_block()?;
            if self.curr_is_ty(TokenType::Semicolon) {
                self.advance();
            }
            Ok(Some(ast::Stmt::While(self.ast_arena.alloc(ast::While { condition, body, span: self.span_from(start) }))))
        } else if self.curr_is_ty(TokenType::Return) {
            self.expect(TokenType::Return)?;
            let expr = self.parse_expr()?;
            self.expect(TokenType::Semicolon)?;
            Ok(Some(ast::Stmt::Return(self.ast_arena.alloc(ast::Return { expr, span: self.span_from(start) }))))
        } else {
            Ok(None)
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<ast::Expr<'ast>> {
        self.parse_expr_add()
    }

    fn parse_expr_add(&mut self) -> ParseResult<ast::Expr<'ast>> {
        let start = self.start();
        let mut left = self.parse_expr_mul()?;
        loop {
            let op = if self.curr_is_ty(TokenType::Plus) {
                ast::BinaryOp::Add
            } else if self.curr_is_ty(TokenType::Minus) {
                ast::BinaryOp::Sub
            } else {
                break;
            };

            self.advance();
            let right = self.parse_expr()?;
            left = ast::Expr::Binary(self.ast_arena.alloc(
                ast::Binary { op, left, right, span: self.span_from(start) },
            ));
        }
        Ok(left)
    }

    fn parse_expr_mul(&mut self) -> ParseResult<ast::Expr<'ast>> {
        let left = self.parse_expr_call()?;
        // loop {
        //     break;
        // }
        Ok(left)
    }

    fn parse_expr_call(&mut self) -> ParseResult<ast::Expr<'ast>> {
        let start = self.start();
        let mut left = self.parse_expr_terminal()?;
        loop {
            if self.curr_is_ty(TokenType::Period) {
                self.advance();
                let attr = self.expect(TokenType::Identifier)?.span;
                left = ast::Expr::Attr(self.ast_arena.alloc(ast::Attr {
                    object: left,
                    attr,
                    span: self.span_from(start)
                }));
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
                self.expect(TokenType::RightParenthesis)?;

                let arguments = self.ast_arena.new_list(arguments);
                left = ast::Expr::Call(self.ast_arena.alloc(ast::Call {
                    callee: left,
                    arguments: self.ast_arena.alloc_slice_fill_iter(arguments),
                    span: self.span_from(start)
                }));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_expr_terminal(&mut self) -> ParseResult<ast::Expr<'ast>> {
        let start = self.start();
        if self.curr_is_ty(TokenType::Identifier) {
            let name = self.expect(TokenType::Identifier)?;
            Ok(ast::Expr::Name(name.span))
        } else if self.curr_is_ty(TokenType::Integer) {
            let constant = self.expect(TokenType::Integer)?;
            Ok(ast::Expr::Integer(constant.span))
        } else if self.curr_is_ty(TokenType::LeftBrace) {
            let block = self.parse_block()?;
            Ok(ast::Expr::Block(self.ast_arena.alloc(block)))
        } else if self.curr_is_ty(TokenType::LeftParenthesis) {
            self.advance();
            let expr = self.parse_expr()?;
            self.expect(TokenType::RightParenthesis)?;
            Ok(expr)
        } else if self.curr_is_ty(TokenType::If) {
            self.advance();
            let condition = if self.curr_is_ty(TokenType::LeftParenthesis) {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenType::RightParenthesis)?;
                expr
            } else {
                self.parse_expr()?
            };
            let then_do = self.parse_expr()?;
            let else_do = if self.curr_is_ty(TokenType::Else) {
                self.advance();
                let expr = self.parse_expr()?;
                Some(expr)
            } else {
                None
            };
            Ok(self.ast_arena.new_node(ast::Expr::IfElse(ast::IfElse {
                condition,
                then_do,
                else_do,
                span: self.span_from(start),
            })))
        } else {
            self.error_expected_none()
        }
    }

    fn parse_type(&mut self) -> ParseResult<ast::Type<'ast>> {
        let token = self.expect(TokenType::Identifier)?;
        Ok(ast::Type(token.span, PhantomData))
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use triomphe::Arc;

    use crate::driver::FrontendDriver;
    use crate::parser::Parse;
    use crate::source::{SourceInput, Sources};

    fn render_ast(text: Arc<str>, _name: String) -> String {
        let driver = FrontendDriver::new();
        let handle = driver.get_handle();
        let ast = driver.block_on(async {
            let source = *handle.query::<Sources>(SourceInput::String(text)).await.unwrap();
            handle.query::<Parse>(source).await.unwrap()
        });
        ast.pretty(2)
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
