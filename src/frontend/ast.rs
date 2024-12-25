use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::ptr::NonNull;
use std::sync::Arc;
use bumpalo::Bump;
use crate::frontend::{InternedString, StringsTable};
use crate::frontend::source::Location;

pub struct AstRef<T>(u32, PhantomData<T>);

impl<T> Clone for AstRef<T> {
    #[inline]
    fn clone(&self) -> Self {
        AstRef(self.0, self.1)
    }
}

impl<T> Copy for AstRef<T> { }

pub struct AstListRef<T>(u32, PhantomData<T>);

impl<T> Clone for AstListRef<T> {
    #[inline]
    fn clone(&self) -> Self {
        AstListRef(self.0, self.1)
    }
}

impl<T> Copy for AstListRef<T> { }

impl<T> AstRef<T> {
    #[inline(always)]
    fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

pub struct AST {
    strings: Arc<StringsTable>,
    arena: Bump,
    nodes: Vec<NonNull<()>>,
    locations: Vec<Location>,
    lists: Vec<(usize, NonNull<()>)>,

    file: AstRef<File>
}

impl AST {
    pub(super) fn new<F, E>(strings: Arc<StringsTable>, try_build: F) -> Result<AST, E> where F: for<'a> FnOnce(&'a StringsTable, &'a mut ASTBuilder) -> Result<AstRef<File>, E> {
        let mut allocator = ASTBuilder {
            arena: Bump::new(),
            nodes: Vec::new(),
            locations: Vec::new(),
            lists: Vec::new()
        };
        let file = try_build(&strings, &mut allocator)?;
        Ok(AST { strings, arena: allocator.arena, nodes: allocator.nodes, locations: allocator.locations, lists: allocator.lists, file })
    }
}

fn index_to_ref<T, O>(ref_fn: fn(u32, PhantomData<T>) -> O, index: usize) -> O {
    ref_fn(index.try_into().unwrap(), PhantomData)
}

pub(super) struct ASTBuilder {
    arena: Bump,
    nodes: Vec<NonNull<()>>,
    locations: Vec<Location>,
    lists: Vec<(usize, NonNull<()>)>
}

impl ASTBuilder {
    pub fn get_location<T>(&self, node: AstRef<T>) -> Location {
        self.locations[node.as_usize()]
    }
    
    pub fn new_list<I, T>(&mut self, items: I) -> AstListRef<T> where I: IntoIterator<Item=T>, I::IntoIter: ExactSizeIterator {
        let items = self.arena.alloc_slice_fill_iter(items);
        let index = index_to_ref(AstListRef, self.lists.len());
        self.lists.push((items.len(), NonNull::new(items.as_mut_ptr()).unwrap().cast()));
        index
    }
    
    pub fn new_node<T>(&mut self, node: T, location: Location) -> AstRef<T> { 
        let item = self.arena.alloc(node);
        let index = index_to_ref(AstRef, self.nodes.len());
        self.nodes.push(NonNull::from(item).cast());
        self.locations.push(location);
        index
    }
}

pub struct File {
    pub top_levels: AstListRef<TopLevel>
}

pub enum TopLevel {
    Function(AstRef<Function>)
}

pub struct Function {
    pub name: InternedString,
    pub return_type: AstRef<Type>,
    pub body: AstRef<Block>
}

pub struct Block {
    pub stmts: AstListRef<AstRef<Stmt>>,
    pub trailing_expr: Option<AstRef<Expr>>,
}

pub enum Stmt {
    Return(AstRef<Expr>),
    Expr(AstRef<Expr>)
}

pub enum Expr {
    Bool(bool),
    Float(f64),
    Integer(i64),
    String(InternedString),
    Name(InternedString),
    Attr {
        object: AstRef<Expr>,
        name: InternedString
    },
    Index {
        object: AstRef<Expr>,
        index: AstRef<Expr>
    },
    Unary {
        op: UnaryOp,
        right: AstRef<Expr>
    },
    Binary {
        op: BinaryOp,
        left: AstRef<Expr>,
        right: AstRef<Expr>
    },
    Call {
        callee: AstRef<Expr>,
        arguments: AstListRef<AstRef<Expr>>,
    },
}

pub enum UnaryOp {
    Not,
    Neg
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}


pub enum Type {
    Name(InternedString)
}
