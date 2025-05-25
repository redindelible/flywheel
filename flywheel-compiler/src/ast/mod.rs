pub mod function;
pub mod ast_struct;
pub mod import;
pub mod statement;
pub mod block;
pub mod attribute;
pub mod index;
pub mod unary;
pub mod binary;
pub mod if_else;
pub mod call;
pub mod ast_type;

use crate::ast::ast_struct::Struct;
use crate::ast::attribute::Attribute;
use crate::ast::binary::{Binary, BinaryOp};
use crate::ast::block::Block;
use crate::ast::call::Call;
use crate::ast::function::Function;
use crate::ast::if_else::IfElse;
use crate::ast::import::Import;
use crate::ast::index::Index;
use crate::ast::unary::{Unary, UnaryOp};
pub(crate) use crate::file_ast::FileAST;
use crate::id::AstId;
use crate::id::beacon::Beacon;
use crate::id::has_id::HasId;
use crate::located::Located;
use crate::source::Location;
use crate::table::AstTable;
use crate::utils::{InternedString};
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub enum TopLevel {
    Function(Beacon<Function>),
    Struct(Beacon<Struct>),
    Import(Beacon<Import>),
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
impl Located for TopLevel {
    fn locate(&self, locations: &AstTable<Location>) -> Option<Location> {
        Self::locate_in(self.id(), locations)
    }
}
impl HasId for TopLevel {
    fn id(&self) -> AstId {
        match self {
            TopLevel::Function(function) => function.id(),
            TopLevel::Struct(struct_) => struct_.id(),
            TopLevel::Import(import) => import.id(),
        }
    }
}

pub enum Expression {
    Bool(Beacon<bool>),
    Float(Beacon<f64>),
    Integer(Beacon<i64>),
    String(Beacon<InternedString>),
    Name(Beacon<InternedString>),
    Block(Beacon<Block>),
    Attribute(Beacon<Attribute>),
    Index(Beacon<Index>),
    Unary(Beacon<Unary>),
    Binary(Beacon<Binary>),
    IfElse(Beacon<IfElse>),
    Call(Beacon<Call>),
}

impl Expression {

    // All convenience methods to make working with boxes make sense.
    pub fn block(block: Block) -> Expression {
        Expression::Block(Beacon::new(block))
    }
    pub fn attribute(object: Expression, name: InternedString) -> Expression {
        Expression::Attribute(Beacon::new(Attribute {
            object,
            name: Beacon::new(name),
        }))
    }
    pub fn index(object: Expression, index: Expression) -> Expression {
        Expression::Index(Beacon::new(Index { object, index }))
    }
    pub fn unary(op: Beacon<UnaryOp>, right: Expression) -> Expression {
        Expression::Unary(Beacon::new(Unary { op, right }))
    }
    pub fn binary(op: Beacon<BinaryOp>, left: Expression, right: Expression) -> Expression {
        Expression::Binary(Beacon::new(Binary { op, left, right }))
    }
    pub fn if_else(condition: Expression, then_do: Expression, else_do: Option<Expression>) -> Expression {
        Expression::IfElse(Beacon::new(IfElse {
            condition,
            then_do,
            else_do,
        }))
    }
    pub fn call(callee: Expression, arguments: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::Call(Beacon::new(Call {
            callee,
            arguments: arguments.into_iter().collect(),
        }))
    }
}
impl HasId for Expression {
    fn id(&self) -> AstId {
        match self {
            Expression::Bool(b) => b.id(),
            Expression::Float(b) => b.id(),
            Expression::Integer(b) => b.id(),
            Expression::String(b) => b.id(),
            Expression::Name(b) => b.id(),
            Expression::Block(b) => b.id(),
            Expression::Attribute(b) => b.id(),
            Expression::Index(b) => b.id(),
            Expression::Unary(b) => b.id(),
            Expression::Binary(b) => b.id(),
            Expression::IfElse(b) => b.id(),
            Expression::Call(b) => b.id(),
        }
    }
}
impl AsDebugTree for Expression {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let location = context.get_location(self.id()).unwrap();
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
                let text = context.strings.resolve(**interned);
                PrettyTree::from_struct("String", [
                    ("value", PrettyTree::from_string(format!("{:?}", text))),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Name(interned) => {
                let text = context.strings.resolve(**interned);
                PrettyTree::from_struct("Name", [
                    ("value", PrettyTree::from_string(format!("{:?}", text))),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Expression::Attribute(attribute) => attribute.as_debug_tree(context),
            Expression::Index(index) => index.as_debug_tree(context),
            Expression::Unary(unary) => unary.as_debug_tree(context), 
            Expression::Binary(binary) => binary.as_debug_tree(context),
            Expression::Call(call) => call.as_debug_tree(context),
            Expression::Block(block) => block.as_debug_tree(context),
            Expression::IfElse(if_else) => if_else.as_debug_tree(context),
        }
    }
}
impl Located for Expression {
    fn locate(&self, locations: &AstTable<Location>) -> Option<Location> {
        Self::locate_in(self.id(), locations)
    }
}