use crate::file_ast::FileAST;
use crate::source::{Location};
use crate::utils::{InternedString};
use crate::utils::located::{Beacon, Located};
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub enum TopLevel {
    Function(Function),
    Struct(Struct),
    Import(Import),
}

pub struct Function {
    pub name: InternedString,
    pub return_type: Beacon<Type>,
    pub body: Block,
    pub location: Location
}

pub struct Struct {
    pub name: InternedString,
    pub fields: Vec<StructField>,
    pub location: Location
}

pub struct Import {
    pub relative_path: InternedString,
    pub location: Location
}

pub struct StructField {
    pub name: InternedString,
    pub ty: Beacon<Type>,
    pub location: Location
}

pub struct Block {
    pub statements: Vec<Beacon<Statement>>,
    pub trailing_expr: Option<Expression>,
    pub location: Location
}

pub enum Statement {
    Let { name: Beacon<InternedString>, ty: Option<Beacon<Type>>, value: Expression },
    While { condition: Expression, body: Block },
    Return(Expression),
    Expr(Expression),
}

impl Located for Statement {
    fn location(&self) -> Location {
        match self {
            Statement::Let { name, value, .. } => name.location.combine(value.location()),
            Statement::While { condition, body } => condition.location().combine(body.location),
            Statement::Return(e) | Statement::Expr(e) => e.location(),
        }
    }
}

pub enum Expression {
    Bool(Beacon<bool>),
    Float(Beacon<f64>),
    Integer(Beacon<i64>),
    String(Beacon<InternedString>),
    Name(Beacon<InternedString>),
    Block(Box<Block>),
    Attr { object: Box<Expression>, name: InternedString },
    Index { object: Box<Expression>, index: Box<Expression> },
    Unary { op: Beacon<UnaryOp>, right: Box<Expression> },
    Binary { op: Beacon<BinaryOp>, left: Box<Expression>, right: Box<Expression> },
    IfElse { condition: Box<Expression>, then_do: Box<Expression>, else_do: Option<Box<Expression>> },
    Call { callee: Box<Expression>, arguments: Vec<Expression> },
}

impl Expression {

    // All convince methods to make working with boxes make sense.
    pub fn block(block: Block) -> Expression {
        Expression::Block(Box::new(block))
    }
    pub fn attribute(object: Expression, name: InternedString) -> Expression {
        Expression::Attr {
            object: Box::new(object),
            name,
        }
    }
    pub fn index(object: Expression, index: Expression) -> Expression {
        Expression::Index { object: Box::new(object), index: Box::new(index) }
    }
    pub fn unary(op: Beacon<UnaryOp>, right: Expression) -> Expression {
        Expression::Unary { op, right: Box::new(right) }
    }
    pub fn binary(op: Beacon<BinaryOp>, left: Expression, right: Expression) -> Expression {
        Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
    pub fn if_else(condition: Expression, then_do: Expression, else_do: Option<Expression>) -> Expression {
        Expression::IfElse {
            condition: Box::new(condition),
            then_do: Box::new(then_do),
            else_do: else_do.map(|x| Box::new(x)),
        }
    }
    pub fn call(callee: Expression, arguments: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(callee),
            arguments: arguments.into_iter().collect(),
        }
    }
}

impl Located for Expression {
    fn location(&self) -> Location {
        match self {
            Expression::Bool(t) => t.location,
            Expression::Float(t) => t.location,
            Expression::Integer(t) => t.location,
            Expression::String(t) => t.location,
            Expression::Name(t) => t.location,
            Expression::Block(t) => t.location,
            Expression::Attr { object, .. } => object.location(),
            Expression::Index { object, ..} => object.location(),
            Expression::Unary { op, right } => op.location.combine(right.location()),
            Expression::Binary { left, right, .. } => left.location().combine(right.location()),
            Expression::IfElse { condition, then_do, else_do } => {
                let mandatory = condition.location().combine(then_do.location());
                if let Some(else_do) = else_do {
                    mandatory.combine(else_do.location())
                } else {
                    mandatory
                }
            }
            Expression::Call { callee, .. } => {
                callee.location()
            }
        }
    }
}

pub enum UnaryOp {
    Not,
    Neg,
}

impl UnaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            UnaryOp::Not => "Not",
            UnaryOp::Neg => "Neg",
        }
    }
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            BinaryOp::Add => "Add",
            BinaryOp::Sub => "Sub",
            BinaryOp::Mul => "Mul",
            BinaryOp::Div => "Div",
            BinaryOp::Mod => "Mod",
        }
    }
}

pub enum Type {
    Name(InternedString),
}

impl AsDebugTree for TopLevel {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        match self {
            TopLevel::Function(function) => function.as_debug_tree(context),
            TopLevel::Struct(struct_) => struct_.as_debug_tree(context),
            TopLevel::Import(import) => import.as_debug_tree(context),
        }
    }
}

impl AsDebugTree for Function {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let name = PrettyTree::from_string(format!("{:?}", context.strings.resolve(self.name)));
        let return_type = self.return_type.as_debug_tree(context);
        let body = self.body.as_debug_tree(context);
        PrettyTree::from_struct("Function", [
            ("name", name),
            ("return_type", return_type),
            ("body", body),
            ("location", PrettyTree::from_location(self.location)),
        ])
    }
}

impl AsDebugTree for Struct {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let name = PrettyTree::from_string(format!("{:?}", context.strings.resolve(self.name)));
        let fields = PrettyTree::from_list(self.fields.iter().map(|field| field.as_debug_tree(context)));
        PrettyTree::from_struct("Struct", [
            ("name", name),
            ("fields", fields),
            ("location", PrettyTree::from_location(self.location)),
        ])
    }
}

impl AsDebugTree for Import {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let relative_path = PrettyTree::from_string(format!("{:?}", context.strings.resolve(self.relative_path)));
        PrettyTree::from_struct("Import", [
            ("relative_path", relative_path),
            ("location", PrettyTree::from_location(self.location)),
        ])
    }
}

impl AsDebugTree for StructField {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let name = PrettyTree::from_string(format!("{:?}", context.strings.resolve(self.name)));
        let ty = self.ty.as_debug_tree(context);
        PrettyTree::from_struct("Field", [
            ("name", name),
            ("ty", ty),
            ("location", PrettyTree::from_location(self.location)),
        ])
    }
}

impl AsDebugTree for Block {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let stmts = PrettyTree::from_list(self.statements.iter().map(|statement| statement.as_debug_tree(context)));
        let trailing_expr = PrettyTree::from_option(self.trailing_expr.as_ref().map(|expr| expr.as_debug_tree(context)));

        PrettyTree::from_struct("Block", [
            ("stmts", stmts),
            ("trailing_expr", trailing_expr),
            ("location", PrettyTree::from_location(self.location)),
        ])
    }
}

impl AsDebugTree for Statement {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let location = self.location();
        match &self {
            Statement::Return(expr) => PrettyTree::from_struct("Return", [
                ("value", expr.as_debug_tree(context)),
                ("location", PrettyTree::from_location(location)),
            ]),
            Statement::Expr(expr) => PrettyTree::from_struct("Expr", [
                ("value", expr.as_debug_tree(context)),
                ("location", PrettyTree::from_location(location)),
            ]),
            Statement::Let { name, ty, value } => {
                let text = context.strings.resolve(name.element);
                let ty = PrettyTree::from_option(ty.as_ref().map(|ty| ty.as_debug_tree(context)));
                let value = value.as_debug_tree(context);
                PrettyTree::from_struct("Let", [
                    ("name", PrettyTree::from_string(format!("{:?}", text))),
                    ("ty", ty),
                    ("value", value),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Statement::While { condition, body } => {
                let condition = condition.as_debug_tree(context);
                let body = body.as_debug_tree(context);
                PrettyTree::from_struct("While", [
                    ("condition", condition),
                    ("body", body),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
        }
    }
}

impl AsDebugTree for Expression {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let location = self.location();
        match &self {
            Expression::Bool(value) => PrettyTree::from_struct("Bool", [
                ("value", PrettyTree::from_string(format!("{}", value))),
                ("location", PrettyTree::from_location(location)),
            ]),
            Expression::Float(value) => PrettyTree::from_struct("Float", [
                ("value", PrettyTree::from_string(format!("{}", value))),
                ("location", PrettyTree::from_location(location)),
            ]),
            Expression::Integer(value) => PrettyTree::from_struct("Integer", [
                ("value", PrettyTree::from_string(format!("{}", value))),
                ("location", PrettyTree::from_location(location)),
            ]),
            Expression::String(interned) => {
                let text = context.strings.resolve(interned.element);
                PrettyTree::from_struct("String", [
                    ("value", PrettyTree::from_string(format!("{:?}", text))),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Name(interned) => {
                let text = context.strings.resolve(interned.element);
                PrettyTree::from_struct("Name", [
                    ("value", PrettyTree::from_string(format!("{:?}", text))),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Attr { object, name } => {
                let object = object.as_debug_tree(context);
                let name = context.strings.resolve(*name);
                PrettyTree::from_struct("Attr", [
                    ("object", object),
                    ("name", PrettyTree::from_string(format!("{:?}", name))),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Index { object, index } => {
                let object = object.as_debug_tree(context);
                let index = index.as_debug_tree(context);
                PrettyTree::from_struct("Index", [
                    ("object", object),
                    ("index", index),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Unary { op, right } => {
                let right = right.as_debug_tree(context);
                PrettyTree::from_struct("Unary", [
                    ("op", PrettyTree::from_string(op.name())),
                    ("right", right),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Binary { left, op, right } => {
                let left = left.as_debug_tree(context);
                let right = right.as_debug_tree(context);
                PrettyTree::from_struct("Binary", [
                    ("left", left),
                    ("op", PrettyTree::from_string(op.name())),
                    ("right", right),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Call { callee, arguments } => {
                let arguments = arguments;
                let arguments = PrettyTree::from_list(arguments.iter().map(|arg| arg.as_debug_tree(context)));
                let callee = callee.as_debug_tree(context);
                PrettyTree::from_struct("Call", [
                    ("callee", callee),
                    ("arguments", arguments),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Block(block) => block.as_debug_tree(context),
            Expression::IfElse { condition, then_do, else_do } => {
                let condition = condition.as_debug_tree(context);
                let then_do = then_do.as_debug_tree(context);
                let else_do = PrettyTree::from_option(else_do.as_ref().map(|expr| expr.as_debug_tree(context)));
                PrettyTree::from_struct("IfElse", [
                    ("condition", condition),
                    ("then_do", then_do),
                    ("else_do", else_do),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
        }
    }
}

impl AsDebugTree for Beacon<Type> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        match &self.element {
            Type::Name(name) => {
                let text = context.strings.resolve(*name);
                PrettyTree::from_struct("Name", [
                    ("name", PrettyTree::from_string(format!("{:?}", text))),
                    ("location", PrettyTree::from_location(self.location)),
                ])
            }
        }
    }
}