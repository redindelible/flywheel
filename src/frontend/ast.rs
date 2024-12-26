use std::marker::PhantomData;
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
    _arena: Bump,
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
        Ok(AST { strings, _arena: allocator.arena, nodes: allocator.nodes, locations: allocator.locations, lists: allocator.lists, file })
    }

    pub fn get_location<T>(&self, node: AstRef<T>) -> Location {
        self.locations[node.as_usize()]
    }
    
    pub fn get<T>(&self, node: AstRef<T>) -> &T {
        let ptr = self.nodes[node.as_usize()];
        unsafe { ptr.cast::<T>().as_ref() }
    }
    
    pub fn get_list<T>(&self, list: AstListRef<T>) -> &[T] {
        let (length, ptr) = self.lists[list.0 as usize];
        unsafe { std::slice::from_raw_parts(ptr.cast::<T>().as_ptr(), length) }
    }

    fn to_tree<T: Pretty>(&self, node: AstRef<T>) -> PrettyNode {
        let location = self.get_location(node);
        self.get(node).to_tree(self, location)
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
    Function(AstRef<Function>),
    Struct(AstRef<Struct>)
}

pub struct Function {
    pub name: InternedString,
    pub return_type: AstRef<Type>,
    pub body: AstRef<Block>
}

pub struct Struct {
    pub name: InternedString,
    pub fields: AstListRef<AstRef<StructField>>
}

pub struct StructField {
    pub name: InternedString,
    pub ty: AstRef<Type>,
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

impl UnaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            UnaryOp::Not => "Not",
            UnaryOp::Neg => "Neg"
        }
    }
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

impl BinaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            BinaryOp::Add => "Add",
            BinaryOp::Sub => "Sub",
            BinaryOp::Mul => "Mul",
            BinaryOp::Div => "Div",
            BinaryOp::Mod => "Mod"
        }
    }
}

pub enum Type {
    Name(InternedString)
}

enum PrettyNode {
    StructBranch {
        name: String,
        children: Vec<(String, PrettyNode)>
    },
    TupleStructBranch {
        name: String,
        children: Vec<PrettyNode>
    },
    ListBranch { children: Vec<PrettyNode> },
    Leaf(String)
}

impl PrettyNode {
    fn from_struct<S>(name: impl Into<String>, children: impl IntoIterator<Item=(S, PrettyNode)>) -> PrettyNode where S: Into<String> {
        let name = name.into();
        let children = children.into_iter().map(|(s, n)| (s.into(), n)).collect();
        PrettyNode::StructBranch { name, children }
    }
    
    fn from_tuple_struct(name: impl Into<String>, children: impl IntoIterator<Item=PrettyNode>) -> PrettyNode {
        PrettyNode::TupleStructBranch { name: name.into(), children: children.into_iter().collect() }
    }
    
    fn from_list(items: impl IntoIterator<Item=PrettyNode>) -> PrettyNode {
        PrettyNode::ListBranch { children: items.into_iter().collect() }
    }
    
    fn from_option(maybe: Option<PrettyNode>) -> PrettyNode {
        match maybe {
            Some(node) => PrettyNode::from_tuple_struct("Some", [node]),
            None => PrettyNode::from_string("None")
        }
    }
    
    fn from_string(text: impl Into<String>) -> PrettyNode {
        PrettyNode::Leaf(text.into())
    }
    
    fn from_location(location: Location) -> PrettyNode {
        PrettyNode::from_string(format!("{location:?}"))
    }
}

trait Pretty {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode;
}

impl Pretty for File {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        let top_levels = PrettyNode::from_list(context.get_list(self.top_levels).iter().map(|top_level| top_level.to_tree(context)));
        PrettyNode::from_struct("File", [
            ("top_levels", top_levels),
            ("location", PrettyNode::from_location(location))
        ])
    }
}

impl TopLevel {
    fn to_tree(&self, context: &AST) -> PrettyNode {
        match self {
            TopLevel::Function(function) => context.to_tree(*function),
            TopLevel::Struct(struct_) => context.to_tree(*struct_),
        }
    }
}

impl Pretty for Function {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        let name = PrettyNode::from_string(format!("{:?}", context.strings.resolve(self.name).unwrap()));
        let return_type = context.to_tree(self.return_type);
        let body = context.to_tree(self.body);
        PrettyNode::from_struct("Function", [
            ("name", name),
            ("return_type", return_type),
            ("body", body),
            ("location", PrettyNode::from_location(location))
        ])
    }
}

impl Pretty for Struct {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        let name = PrettyNode::from_string(format!("{:?}", context.strings.resolve(self.name).unwrap()));
        let fields = PrettyNode::from_list(context.get_list(self.fields).iter().map(|field| context.to_tree(*field)));
        PrettyNode::from_struct("Struct", [
            ("name", name),
            ("fields", fields),
            ("location", PrettyNode::from_location(location))
        ])
    }
}

impl Pretty for StructField {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        let name = PrettyNode::from_string(format!("{:?}", context.strings.resolve(self.name).unwrap()));
        let ty = context.to_tree(self.ty);
        PrettyNode::from_struct("Field", [
            ("name", name),
            ("ty", ty),
            ("location", PrettyNode::from_location(location))
        ])
    }
}

impl Pretty for Block {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        let stmts = PrettyNode::from_list(context.get_list(self.stmts).iter().map(|stmt| context.to_tree(*stmt)));
        let trailing_expr = PrettyNode::from_option(self.trailing_expr.map(|expr| context.to_tree(expr)));
        
        PrettyNode::from_struct("Block", [
            ("stmts", stmts),
            ("trailing_expr", trailing_expr),
            ("location", PrettyNode::from_location(location))
        ])
    }
}

impl Pretty for Stmt {fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        match self {
            Stmt::Return(expr) => {
                PrettyNode::from_struct("Return", [
                    ("value", context.to_tree(*expr)),
                    ("location", PrettyNode::from_location(location))
                ])
            }
            Stmt::Expr(expr) => {
                PrettyNode::from_struct("Expr", [
                    ("value", context.to_tree(*expr)),
                    ("location", PrettyNode::from_location(location))
                ])
            }
        }
    }
}

impl Pretty for Expr {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        match self {
            Expr::Bool(value) => {
                PrettyNode::from_struct("Bool", [
                    ("value", PrettyNode::from_string(format!("{}", value))),
                    ("location", PrettyNode::from_location(location))
                ])
            },
            Expr::Float(value) => {
                PrettyNode::from_struct("Float", [
                    ("value", PrettyNode::from_string(format!("{}", value))),
                    ("location", PrettyNode::from_location(location))
                ])
            },
            Expr::Integer(value) => {
                PrettyNode::from_struct("Integer", [
                    ("value", PrettyNode::from_string(format!("{}", value))),
                    ("location", PrettyNode::from_location(location))
                ])
            },
            Expr::String(interned) => {
                let text = context.strings.resolve(*interned).unwrap();
                PrettyNode::from_struct("String", [
                    ("value", PrettyNode::from_string(format!("{:?}", text))),
                    ("location", PrettyNode::from_location(location))
                ])
            }
            Expr::Name(interned) => {
                let text = context.strings.resolve(*interned).unwrap();
                PrettyNode::from_struct("Name", [
                    ("value", PrettyNode::from_string(format!("{:?}", text))),
                    ("location", PrettyNode::from_location(location))
                ])
            }
            Expr::Attr { object, name } => {
                let object = context.to_tree(*object);
                let name = context.strings.resolve(*name).unwrap();
                PrettyNode::from_struct("Attr", [
                    ("object", object),
                    ("name", PrettyNode::from_string(format!("{:?}", name))),
                    ("location", PrettyNode::from_location(location))
                ])
            }
            Expr::Index { object, index } => {
                let object = context.to_tree(*object);
                let index = context.to_tree(*index);
                PrettyNode::from_struct("Index", [
                    ("object", object),
                    ("index", index),
                    ("location", PrettyNode::from_location(location))
                ])
            }
            Expr::Unary { op, right } => {
                let right = context.to_tree(*right);
                PrettyNode::from_struct("Unary", [
                    ("op", PrettyNode::from_string(op.name())),
                    ("right", right),
                    ("location", PrettyNode::from_location(location))
                ])
            }
            Expr::Binary { left, op, right } => {
                let left = context.to_tree(*left);
                let right = context.to_tree(*right);
                PrettyNode::from_struct("Binary", [
                    ("left", left),
                    ("op", PrettyNode::from_string(op.name())),
                    ("right", right),
                    ("location", PrettyNode::from_location(location))
                ])
            }
            Expr::Call { callee, arguments } => {
                let arguments = context.get_list(*arguments);
                let arguments = PrettyNode::from_list(arguments.iter().map(|arg| context.to_tree(*arg)));
                let callee = context.to_tree(*callee);
                PrettyNode::from_struct("Call", [
                    ("callee", callee),
                    ("arguments", arguments),
                    ("location", PrettyNode::from_location(location))
                ])
            }
        }
    }
}

impl Pretty for Type {
    fn to_tree(&self, context: &AST, location: Location) -> PrettyNode {
        match self {
            Type::Name(name) => {
                let text = context.strings.resolve(*name).unwrap();
                PrettyNode::from_struct("Name", [
                    ("name", PrettyNode::from_string(format!("{:?}", &*text))),
                    ("location", PrettyNode::from_location(location))
                ])
            }
        }
    }
}

struct PrettyOptions {
    indent: String,
    max_width: usize,
}

impl PrettyNode {
    fn render_delimited<'a>(start: &str, end: &str, children: impl IntoIterator<Item=(String, &'a PrettyNode)> + 'a, options: &PrettyOptions, line_prefix: &str) -> String {
        let mut output = start.to_string();
        let child_line_prefix = String::from_iter([line_prefix, options.indent.as_str()]);
        let mut is_first = true;
        for (field_prefix, field) in children {
            if !is_first { output.push(','); } else {  is_first = false; }
            output.push('\n');
            output.push_str(&child_line_prefix);
            output.push_str(&field_prefix);
            output.push_str(&field.render(options, &child_line_prefix));
        }
        output.push('\n');
        output.push_str(line_prefix);
        output.push_str(end.into());
        output
    }
    
    fn render(&self, options: &PrettyOptions, line_prefix: &str) -> String {
        match self {
            PrettyNode::StructBranch { name, children } => {
                PrettyNode::render_delimited(
                    &format!("{} {{", name),
                    "}",
                    children.iter().map(|(field_name, field)| {
                        (format!("{}: ", field_name), field)
                    }),
                    options,
                    line_prefix
                )
            }
            PrettyNode::TupleStructBranch { name, children } => {
                PrettyNode::render_delimited(
                    &format!("{} (", name),
                    ")",
                    children.iter().map(|field| {
                        (String::new(), field)
                    }),
                    options,
                    line_prefix
                )
            }
            PrettyNode::ListBranch { children } => {
                PrettyNode::render_delimited(
                    "[",
                    "]",
                    children.iter().map(|field| {
                        (String::new(), field)
                    }),
                    options,
                    line_prefix
                )
            }
            PrettyNode::Leaf(text) => text.clone()
        }
    }
}

impl AST {
    pub fn pretty(&self, indent: usize) -> String {
        let tree = self.to_tree(self.file);
        tree.render(&PrettyOptions { indent: String::from_iter(std::iter::repeat_n(' ', indent)), max_width: 80}, "")
    }
}
