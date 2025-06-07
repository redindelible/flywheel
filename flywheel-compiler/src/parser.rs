use std::collections::HashSet;
use triomphe::Arc;
use crate::ast;
use crate::ast::ast_struct::Struct;
use crate::ast::ast_type::Type;
use crate::ast::binary::BinaryOp;
use crate::ast::block::Block;
use crate::ast::Expression;
use crate::ast::function::Function;
use crate::ast::import::Import;
use crate::ast::statement::Statement;
use crate::driver::Handle;
use crate::error::{CompileError, CompileResult};
use crate::file_ast::FileAST;
use crate::id::AstId;
use crate::id::beacon::Beacon;
use crate::id::has_id::HasId;
use crate::lexer::{Lexer, LexerShared};
use crate::query::Processor;
use crate::source::{Location, SourceID};
use crate::table::AstTable;
use crate::token::{Token, TokenStream, TokenType};
use crate::utils::Interner;

pub struct Parse(LexerShared);

impl Parse {
    pub fn new(interner: &Arc<Interner>) -> Self {
        Parse(LexerShared::new(interner))
    }
}

impl Processor for Parse {
    type Input = SourceID;
    type Output = FileAST;

    async fn process(handle: Handle, input: SourceID) -> CompileResult<FileAST> {
        let source = handle.get_source(input);
        let strings = handle.processor::<Parse>();

        let lexer = Lexer::new(&strings.0, source.id(), source.text());
        let mut parser = Parser::new(lexer);

        match parser.parse_file() {
            Ok(top_levels) => Ok(FileAST::new(source.id(), strings.0.interner().clone_arc(), top_levels,parser.locations)),
            Err(_) => Err(parser.error.unwrap())
        }
    }
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
            format!(
                "Got {}, expected any of {}, or {}.",
                actual.ty.name(),
                items.iter().map(TokenType::name).collect::<Vec<_>>().join(", "),
                last.name()
            )
        }
    };
    CompileError::with_description_and_location("parse/expected-any-of", description, actual.location)
}

type ParseResult<T> = Result<T, usize>;

struct Parser<L> {
    token_stream: L,
    last: Option<Token>,
    curr: Option<Token>,
    locations: AstTable<Location>,

    possible_tokens: HashSet<TokenType>,
    error: Option<CompileError>,
}

impl<L: TokenStream> Parser<L> {
    fn new(mut lexer: L) -> Self {
        let curr = lexer.next();
        Parser { token_stream: lexer, last: None, curr, possible_tokens: HashSet::new(), error: None, locations: AstTable::new() }
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

    fn copy_current(&self) -> Token {
        self.curr.unwrap_or(Token::new_eof(self.token_stream.source_id()))
    }

    fn current_type(&self) -> TokenType {
        self.curr.map_or(TokenType::Eof, |token| token.ty)
    }

    fn current_is_type(&mut self, ty: TokenType) -> bool {
        self.possible_tokens.insert(ty);
        let curr_ty = self.current_type();
        curr_ty == ty
    }

    fn last_was_ty(&mut self, ty: TokenType) -> bool {
        self.last.is_some_and(|token| token.ty == ty)
    }

    fn error<T>(&mut self, error: CompileError) -> ParseResult<T> {
        self.error = Some(error);
        Err(0)
    }

    /// Tracks the location of `of` by wrapping it in a `Beacon`
    fn track_location<T>(&mut self, of: T, location: Location) -> Beacon<T> {
        let beacon = Beacon::new(of);
        self.locations.insert(beacon.id(), location);
        beacon
    }

    fn fetch_location(&mut self, id: AstId) -> Location {
        *self.locations.get(&id).unwrap()
    }

    fn error_expected_none<T>(&mut self) -> ParseResult<T> {
        let curr = self.curr.unwrap_or(Token::new_eof(self.token_stream.source_id()));
        let tys: Vec<TokenType> = self.possible_tokens.drain().collect();
        self.error(error_expected_any_of(&tys, curr))
    }

    /// Consume the token if it is the same type as `ty`
    fn expect(&mut self, ty: TokenType) -> ParseResult<Token> {
        self.possible_tokens.insert(ty);
        let curr = self.copy_current();
        if curr.ty == ty {
            self.advance();
            Ok(curr)
        } else {
            self.error_expected_none()
        }
    }

    fn parse_file(&mut self) -> ParseResult<Vec<ast::TopLevel>> {
        let mut top_levels = vec![];
        while !self.current_is_type(TokenType::Eof) {
            top_levels.push(self.parse_top_level()?);
        }

        Ok(top_levels)
    }

    fn parse_top_level(&mut self) -> ParseResult<ast::TopLevel> {
        if self.current_is_type(TokenType::Struct) {
            Ok(ast::TopLevel::Struct(self.parse_struct()?))
        } else if self.current_is_type(TokenType::Fn) {
            Ok(ast::TopLevel::Function(self.parse_function()?))
        } else if self.current_is_type(TokenType::Import) {
            Ok(ast::TopLevel::Import(self.parse_import()?))
        } else {
            self.error_expected_none()
        }
    }

    fn parse_import(&mut self) -> ParseResult<Beacon<Import>> {
        let start = self.expect(TokenType::Import)?;
        let path = self.expect(TokenType::String)?.text.unwrap();
        let end = self.expect(TokenType::Semicolon)?;

        Ok(self.track_location(Import { relative_path: path },  start.location.combine(end.location)))
    }

    fn parse_struct(&mut self) -> ParseResult<Beacon<Struct>> {
        let start = self.expect(TokenType::Struct)?;
        let name = self.expect(TokenType::Identifier)?;

        let mut fields = Vec::new();
        self.expect(TokenType::LeftCurlyBrace)?;
        while !self.current_is_type(TokenType::RightCurlyBrace) {
            fields.push(self.parse_field()?);
        }
        let end = self.expect(TokenType::RightCurlyBrace)?;

        let name = self.track_location(name.text.unwrap(), name.location);
        Ok(self.track_location(Struct { name, fields, }, start.location.combine(end.location)))
    }

    fn parse_field(&mut self) -> ParseResult<Beacon<ast::ast_struct::Field>> {
        let name = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let ty = self.parse_type()?;
        let end = self.expect(TokenType::Semicolon)?;
        let location = name.location.combine(end.location);

        let name = self.track_location(name.text.unwrap(), name.location);
        Ok(self.track_location(ast::ast_struct::Field { name, ty }, location))
    }

    fn parse_function(&mut self) -> ParseResult<Beacon<Function>> {
        let start = self.expect(TokenType::Fn)?;
        let name_token = self.expect(TokenType::Identifier)?;
        let name = name_token.text.unwrap();

        self.expect(TokenType::LeftParenthesis)?;
        self.expect(TokenType::RightParenthesis)?;
        self.expect(TokenType::LeftArrow)?;

        let return_type = self.parse_type()?;
        let body = self.parse_block()?;
        let loc = start.location.combine(self.fetch_location(return_type.id()));

        let name = self.track_location(name, name_token.location);
        Ok(self.track_location(Function { name, return_type, body}, loc))
    }

    fn parse_block(&mut self) -> ParseResult<Beacon<Block>> {
        let start = self.expect(TokenType::LeftCurlyBrace)?;

        let mut statements = Vec::new();
        let mut trailing_expr: Option<Expression> = None;

        while !self.current_is_type(TokenType::RightCurlyBrace) {
            if let Some(expr) = trailing_expr.take() {
                let location = self.fetch_location(expr.id());
                statements.push(self.track_location(Statement::Expr(expr), location));
            }

            let maybe_stmt = self.parse_statement()?;
            if let Some(stmt) = maybe_stmt {
                statements.push(stmt);
            } else {
                let expr = self.parse_expr()?;
                trailing_expr = Some(expr);
            }
        }
        let end = self.expect(TokenType::RightCurlyBrace)?;
        Ok(self.track_location(Block { statements, trailing_expr}, start.location.combine(end.location)))
    }
    fn parse_let(&mut self) -> ParseResult<Option<Beacon<Statement>>> {
        let start = self.expect(TokenType::Let)?;
        let name = self.expect(TokenType::Identifier)?;
        let ty = if self.current_is_type(TokenType::Colon) {
            self.expect(TokenType::Colon)?;
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(TokenType::Equal)?;
        let value = self.parse_expr()?;
        let end = self.expect(TokenType::Semicolon)?;
        let name = self.track_location(name.text.unwrap(), name.location);
        Ok(Some(
            self.track_location(Statement::Let {
                name,
                ty,
                value
            }, start.location.combine(end.location))
        ))

    }

    fn parse_while(&mut self) -> ParseResult<Option<Beacon<Statement>>> {
        let start = self.expect(TokenType::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;
        let end = if self.current_is_type(TokenType::Semicolon) {
            self.advance().location
        } else {
            self.fetch_location(body.id())
        };

        Ok(Some(
            self.track_location(Statement::While { condition, body }, start.location.combine(end))
        ))
    }

    fn parse_return(&mut self) -> ParseResult<Option<Beacon<Statement>>> {
        let start = self.expect(TokenType::Return)?;
        let expr = self.parse_expr()?;
        let end = self.expect(TokenType::Semicolon)?;
        Ok(Some(
            self.track_location(Statement::Return(expr), start.location.combine(end.location))
        ))
    }
    fn parse_statement(&mut self) -> ParseResult<Option<Beacon<Statement>>> {
        match self.current_type() {
            TokenType::Let => self.parse_let(),
            TokenType::While => self.parse_while(),
            TokenType::Return => self.parse_return(),
            _ => Ok(None)
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expression> {
        self.parse_expr_add()
    }

    fn parse_expr_add(&mut self) -> ParseResult<Expression> {
        let mut left = self.parse_expr_mul()?;
        loop {
            let maybe_operator = self.copy_current();
            let op = match maybe_operator.ty {
                TokenType::Plus => BinaryOp::Add,
                TokenType::Minus => BinaryOp::Sub,
                _ => break
            };

            self.advance();
            let right = self.parse_expr()?;
            left = Expression::binary(self.track_location(op, maybe_operator.location), left, right)
        }
        Ok(left)
    }

    fn parse_expr_mul(&mut self) -> ParseResult<Expression> {
        let left = self.parse_attribute_access()?;
        // loop {
        //     break;
        // }
        Ok(left)
    }

    fn parse_attribute(&mut self, on: Expression) -> ParseResult<Expression> {
        self.expect(TokenType::Period)?;
        let attribute = self.expect(TokenType::Identifier)?;
        Ok(
            Expression::attribute(on, attribute.text.unwrap())
        )
    }

    fn parse_call(&mut self, callee: Expression) -> ParseResult<Expression> {
        self.advance();
        let mut arguments = Vec::new();
        while !self.current_is_type(TokenType::RightParenthesis) {
            arguments.push(self.parse_expr()?);
            if self.current_is_type(TokenType::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        Ok(Expression::call(callee, arguments))
    }

    fn parse_attribute_access(&mut self) -> ParseResult<Expression> {
        let mut left = self.parse_expr_terminal()?;

        loop {
            left = match self.current_type() {
                TokenType::Period => self.parse_attribute(left)?,
                TokenType::LeftParenthesis => self.parse_call(left)?,
                _ => break
            };
        }
        Ok(left)
    }

    fn parse_identifier(&mut self) -> ParseResult<Expression> {
        let name = self.expect(TokenType::Identifier)?;
        Ok(Expression::Name(self.track_location(name.text.unwrap(), name.location)))
    }

    fn parse_integer(&mut self) -> ParseResult<Expression> {
        let numeric_token = self.expect(TokenType::Integer)?;
        let number = self.token_stream.interner().resolve(numeric_token.text.unwrap()).parse::<i64>();
        match number {
            Ok(n) => Ok(Expression::Integer(self.track_location(n, numeric_token.location))),
            Err(_) => self.error(error_integer_too_big(numeric_token.location))
        }
    }

    fn parse_parenthesized_expression(&mut self) -> ParseResult<ast::Expression> {
        // skip over the left paren
        self.expect(TokenType::LeftParenthesis)?;
        let expr = self.parse_expr()?;
        self.expect(TokenType::RightParenthesis)?;
        Ok(expr)
    }

    fn parse_if(&mut self) -> ParseResult<Expression> {
        // skip over the if.
        self.expect(TokenType::If)?;
        let condition = if self.current_is_type(TokenType::LeftParenthesis) {
            self.advance();
            let expr = self.parse_expr()?;
            self.expect(TokenType::RightParenthesis)?;
            expr
        } else {
            self.parse_expr()?
        };
        let then_do = self.parse_expr()?;
        let else_do = if self.current_is_type(TokenType::Else) {
            self.advance();
            let expr = self.parse_expr()?;
            Some(expr)
        } else {
            None
        };
        Ok(Expression::if_else(condition, then_do, else_do))
    }

    fn parse_expr_terminal(&mut self) -> ParseResult<Expression> {
        match self.current_type() {
            TokenType::Identifier => self.parse_identifier(),
            TokenType::Integer => self.parse_integer(),
            TokenType::LeftCurlyBrace => Ok(Expression::Block(self.parse_block()?)),
            TokenType::LeftParenthesis => self.parse_parenthesized_expression(),
            TokenType::If => self.parse_if(),
            _ => self.error_expected_none()
        }
    }

    fn parse_type(&mut self) -> ParseResult<Beacon<Type>> {
        let token = self.expect(TokenType::Identifier)?;
        Ok(self.track_location(Type::Name(token.text.unwrap()), token.location))
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
