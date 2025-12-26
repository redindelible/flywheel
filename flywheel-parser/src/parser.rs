use std::collections::HashSet;
use std::marker::PhantomData;

use bumpalo::Bump;
use flywheel_ast::{self as ast, File};
use flywheel_sources::{Interner, Source, Span, Symbol};
use flywheel_error::{CompileMessage, CompileResult};

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};


pub fn parse_source(source: &Source, interner: &mut Interner) -> CompileResult<File> {
    File::new(|arena| {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, interner, arena);

        parser.parse_file().map_err(|_| Box::new(parser.error.unwrap()))
    })
}

fn error_expected_any_of(possible: &[TokenType], actual: Token) -> CompileMessage {
    let message = match possible {
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
    CompileMessage::error(message).with_span(actual.span)
}

type ParseResult<T> = Result<T, usize>;

#[derive(Copy, Clone)]
struct Start(usize);

struct Parser<'source, 'ast> {
    tokens: Lexer<'source>,
    curr: Token,
    last_ty: TokenType,
    last_end: usize,

    interner: &'source mut Interner,
    ast_arena: &'ast Bump,

    possible_tokens: HashSet<TokenType>,  // TODO this could probably be a u64
    error: Option<CompileMessage>,
}

impl<'source, 'ast> Parser<'source, 'ast> {
    fn new(lexer: Lexer<'source>, interner: &'source mut Interner, ast_arena: &'ast Bump) -> Self {
        let mut this = Parser {
            curr: lexer.eof(),
            tokens: lexer,
            last_ty: TokenType::Eof,
            last_end: 0,
            interner,
            ast_arena,
            possible_tokens: HashSet::new(),
            error: None
        };
        this.advance();
        this
    }

    fn advance(&mut self) -> Token {
        self.possible_tokens.clear();
        self.last_ty = self.curr.ty;
        self.last_end = self.tokens.span().end;
        self.curr = self.tokens.advance();
        self.curr
    }

    fn start(&self) -> Start {
        Start(self.tokens.span().start)
    }

    fn span_from(&self, point: Start) -> Span {
        self.tokens.source().add_span(point.0..self.last_end)
    }

    fn curr_is_ty(&mut self, ty: TokenType) -> bool {
        self.possible_tokens.insert(ty);
        self.curr.ty == ty
    }

    fn last_was_ty(&mut self, ty: TokenType) -> bool {
        self.last_ty == ty
    }

    fn error<T>(&mut self, error: CompileMessage) -> ParseResult<T> {
        assert!(self.error.is_none());
        self.error = Some(error);
        Err(0)
    }

    fn error_expected_none<T>(&mut self) -> ParseResult<T> {
        let tys: Vec<TokenType> = self.possible_tokens.drain().collect();
        self.error(error_expected_any_of(&tys, self.curr))
    }

    fn expect(&mut self, ty: TokenType) -> ParseResult<Token> {
        if self.curr_is_ty(ty) {
            let ret = self.curr;
            self.advance();
            Ok(ret)
        } else {
            self.error_expected_none()
        }
    }

    fn expect_symbol(&mut self, ty: TokenType) -> ParseResult<Symbol> {
        let token = self.expect(ty)?;
        Ok(self.interner.get_or_intern(token.span))
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
        let top_levels = self.ast_arena.alloc_slice_fill_iter(top_levels);
        Ok(top_levels)
    }

    fn parse_top_level(&mut self) -> ParseResult<ast::TopLevel<'ast>> {
        if self.curr_is_ty(TokenType::Struct) {
            Ok(ast::TopLevel::Struct(self.ast_arena.alloc(self.parse_struct()?)))
        } else if self.curr_is_ty(TokenType::Fn) {
            Ok(ast::TopLevel::Function(self.ast_arena.alloc(self.parse_function()?)))
        } else if self.curr_is_ty(TokenType::From) {
            Ok(ast::TopLevel::Import(self.ast_arena.alloc(self.parse_import()?)))
        } else {
            self.error_expected_none()
        }
    }

    fn parse_import(&mut self) -> ParseResult<ast::Import<'ast>> {
        let start = self.start();
        self.expect(TokenType::From)?;

        let anchor: Option<Symbol>;
        if self.curr_is_ty(TokenType::Identifier) {
            anchor = Some(self.expect_symbol(TokenType::Identifier)?);
        } else {
            anchor = None;
        }

        let mut path = Vec::new();
        while self.curr_is_ty(TokenType::Period) {
            self.advance();
            path.push(self.expect_symbol(TokenType::Identifier)?);
        }
        let path = self.ast_arena.alloc_slice_copy(&path);

        self.expect(TokenType::Import)?;

        let item = self.expect_symbol(TokenType::Identifier)?;
        self.expect(TokenType::Semicolon)?;
        Ok(ast::Import { anchor, path, item, span: self.span_from(start) })
    }

    fn parse_struct(&mut self) -> ParseResult<ast::Struct<'ast>> {
        let start = self.start();
        self.expect(TokenType::Struct)?;
        let name = self.expect_symbol(TokenType::Identifier)?;

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
        let name = self.expect_symbol(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let ty = self.parse_type()?;
        self.expect(TokenType::Semicolon)?;
        Ok(ast::StructField { name, ty, span: self.span_from(start) })
    }

    fn parse_function(&mut self) -> ParseResult<ast::Function<'ast>> {
        let start = self.start();
        self.expect(TokenType::Fn)?;
        let name = self.expect_symbol(TokenType::Identifier)?;
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
            let name = self.expect_symbol(TokenType::Identifier)?;
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
                let attr = self.expect_symbol(TokenType::Identifier)?;
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

                let arguments = self.ast_arena.alloc_slice_fill_iter(arguments);
                left = ast::Expr::Call(self.ast_arena.alloc(ast::Call {
                    callee: left,
                    arguments,
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
            let name = self.expect_symbol(TokenType::Identifier)?;
            Ok(ast::Expr::Name(name))
        } else if self.curr_is_ty(TokenType::Integer) {
            let constant = self.expect_symbol(TokenType::Integer)?;
            Ok(ast::Expr::Integer(constant))
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
            Ok(ast::Expr::IfElse(self.ast_arena.alloc(ast::IfElse {
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
        let token = self.expect_symbol(TokenType::Identifier)?;
        Ok(ast::Type { name: token, phantom: PhantomData })
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;
    use pretty_assertions::assert_str_eq;
    use flywheel_sources::{Interner, SourceMap};
    use crate::parse_source;

    fn ast_test_file(text: &str, expected_ast: &str) {
        let sources = Arc::new(SourceMap::new());
        let mut interner = Interner::new(Arc::clone(&sources));
        let source = sources.add_file("<test>", text.into());

        let file_ast = parse_source(source, &mut interner).unwrap();
        let actual = format!("{:#?}", file_ast.pretty(&sources));

        assert_str_eq!(actual, expected_ast);
    }

    #[test]
    fn test_simple() {
        ast_test_file(include_str!("../test/simple.fly"), include_str!("../test/simple.fly.ast"));
    }

    #[test]
    fn test_simple_return() {
        ast_test_file(include_str!("../test/simple-return.fly"), include_str!("../test/simple-return.fly.ast"));
    }

    // #[test]
    // fn test_simple_struct() {
    //     run_ast_test!("simple-struct.fly");
    // }
    //
    // #[test]
    // fn test_control_flow() {
    //     run_ast_test!("control-flow.fly");
    // }

    #[test]
    fn test_import() {
        ast_test_file(include_str!("../test/import.fly"), include_str!("../test/import.fly.ast"));
    }
}
