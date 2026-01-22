mod namespace;

use std::collections::HashMap;
use std::sync::Arc;

use by_address::ByAddress;
use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::Symbol;
use crate::context::{LoweringContext, WithCollectedNames};
use crate::namespace::{Builtin, Item, Namespace};

enum Type<'ast> {
    U32,
    Struct(&'ast ast::Struct<'ast>),
}

mod context {
    use std::collections::HashMap;
    use std::marker::PhantomData;
    use std::ops::Deref;
    use std::sync::Arc;
    use flywheel_sources::{SourceMap, Symbol};
    use flywheel_ast as ast;
    use crate::{CollectedNames, StructFields, Type};

    #[repr(C)]
    pub struct LoweringContext<'ast, S> {
        _state: PhantomData<S>,

        sources: Arc<SourceMap>,
        ast: &'ast ast::Module,
        builtins: &'ast HashMap<&'static str, Symbol>,

        collected_names: Option<CollectedNames<'ast>>,
        struct_fields: Option<StructFields<'ast>>,
    }

    impl<'ast> LoweringContext<'ast, ()> {
        pub fn new(ast: &'ast ast::Module, builtins: &'ast HashMap<&'static str, Symbol>) -> LoweringContext<'ast, ()> {
            LoweringContext {
                _state: PhantomData,

                sources: Arc::clone(&ast.sources),
                ast,
                builtins,

                collected_names: None,
                struct_fields: None,
            }
        }

        pub fn ast(&self) -> &'ast ast::Module {
            self.ast
        }

        pub fn builtins(&self) -> &'ast HashMap<&'static str, Symbol> {
            self.builtins
        }

        pub fn get_symbol(&self, symbol: Symbol) -> &str {
            self.sources.get_symbol(symbol)
        }

        pub fn with_collected_names(mut self, collected_names: CollectedNames<'ast>) -> LoweringContext<'ast, WithCollectedNames> {
            self.collected_names = Some(collected_names);
            unsafe { std::mem::transmute(self) }
        }
    }

    pub struct WithCollectedNames;

    impl<'ast> Deref for LoweringContext<'ast, WithCollectedNames> {
        type Target = LoweringContext<'ast, ()>;

        fn deref(&self) -> &Self::Target {
            unsafe { std::mem::transmute::<&Self, &Self::Target>(self) }
        }
    }

    impl<'ast> LoweringContext<'ast, WithCollectedNames> {
        pub fn collected_names(&self) -> &CollectedNames<'ast> {
            unsafe { self.collected_names.as_ref().unwrap_unchecked() }
        }

        pub fn with_struct_fields(mut self, struct_fields: StructFields<'ast>) -> LoweringContext<'ast, WithStructFields> {
            self.struct_fields = Some(struct_fields);
            unsafe { std::mem::transmute(self) }
        }
    }

    pub struct WithStructFields;

    impl<'ast> LoweringContext<'ast, WithStructFields> {
        pub fn struct_fields(&self) -> &StructFields<'ast> {
            unsafe { self.struct_fields.as_ref().unwrap_unchecked() }
        }
    }
}

pub struct CollectedNames<'ast> {
    pub file_namespaces: HashMap<&'ast [Symbol], Arc<Namespace<'ast>>>,
    pub prelude_ns: Arc<Namespace<'ast>>,
}

pub struct StructFields<'ast> {
    struct_fields: HashMap<ByAddress<&'ast ast::Struct<'ast>>, HashMap<Symbol, Type<'ast>>>
}

impl<'ast> LoweringContext<'ast, WithCollectedNames> {
    fn resolve_name_as_type(&self, in_ns: &Namespace<'ast>, name: Symbol) -> CompileResult<Type<'ast>> {
        match in_ns.resolve(name) {
            None => {
                let message = format!("No type found called {}", self.get_symbol(name));
                Err(CompileMessage::error(message))
            }
            Some(Item::Function(_)) => {
                let message = format!("No type found called {}", self.get_symbol(name));
                Err(CompileMessage::error(message))
            }
            Some(Item::Imported(import)) => {
                assert!(import.anchor.is_none());
                self.resolve_name_as_type(&self.collected_names().file_namespaces[import.path], name) // todo cache this and prevent cycles
            }
            Some(Item::Struct(struct_)) => Ok(Type::Struct(struct_)),
            Some(Item::Builtin(Builtin::U32)) => Ok(Type::U32),
        }
    }

    fn resolve_type(
        &self,
        in_ns: &Namespace<'ast>,
        ast_ty: &ast::Type<'ast>,
    ) -> CompileResult<Type<'ast>> {
        self.resolve_name_as_type(in_ns, ast_ty.name)
    }
}

pub const BUILTINS: &[&str] = &["u32"];

pub fn lower(ast: &ast::Module, builtins: &HashMap<&'static str, Symbol>) -> CompileResult<()> {
    let ctx = LoweringContext::new(ast, builtins);

    let mut prelude_ns = Namespace::new_root(Arc::clone(&ast.sources));
    prelude_ns.add(builtins["u32"], Item::Builtin(Builtin::U32)).unwrap();
    let prelude_ns = Arc::new(prelude_ns);

    let mut file_namespaces: HashMap<&[Symbol], Arc<Namespace>> = HashMap::new();
    for (path_in_module, file) in &ast.contents {
        let mut file_ns = Namespace::new_child(Arc::clone(&prelude_ns));

        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Import(import) => {
                    file_ns.add(import.item, Item::Imported(import))?;
                }
                ast::TopLevel::Function(func) => {
                    file_ns.add(func.name, Item::Function(func))?;
                }
                ast::TopLevel::Struct(struct_) => {
                    file_ns.add(struct_.name, Item::Struct(struct_))?;
                }
            }
        }
        file_namespaces.insert(path_in_module, Arc::new(file_ns));
    }

    let ctx = ctx.with_collected_names(CollectedNames {
        file_namespaces,
        prelude_ns,
    });

    let mut struct_fields = HashMap::new();
    for (path_in_module, file) in &ast.contents {
        let file_namespace = &*ctx.collected_names().file_namespaces[path_in_module.as_slice()];
        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Struct(struct_) => {
                    let mut fields = HashMap::new();

                    for field in struct_.fields {
                        let ty = ctx.resolve_type(file_namespace, &field.ty)?;
                        fields.insert(field.name, ty);
                    }
                    assert!(struct_fields.insert(ByAddress(struct_), fields).is_none());
                }
                ast::TopLevel::Function(_) => (),
                ast::TopLevel::Import(_) => (),
            };
        }
    }

    let ctx = ctx.with_struct_fields(StructFields {
        struct_fields
    });

    Ok(())
}
