use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::Symbol;

use crate::context::{CollectedNames, CollectedNamesData, LoweringContext};
use crate::namespace::{Builtin, Item, Namespace, SearchPath};
use crate::types::Type;

impl<'ast> LoweringContext<'ast, CollectedNames> {
    pub(crate) fn resolve_name_as_type(&self, search: &SearchPath<'_, 'ast>, name: Symbol) -> CompileResult<Type<'ast>> {
        match search.resolve(name) {
            None => Err(CompileMessage::error_dyn(move |s| format!("No type found called {}", s.get_symbol(name)))),
            Some(Item::Function(_)) => {
                Err(CompileMessage::error_dyn(move |s| format!("No type found called {}", s.get_symbol(name))))
            }
            Some(Item::Imported(import)) => {
                assert!(import.anchor.is_none());
                self.resolve_name_as_type(&SearchPath::from(&self.collected_names().file_namespaces[import.path]), name) // todo cache this and prevent cycles
            }
            Some(Item::Struct(struct_)) => Ok(Type::Struct(struct_)),
            Some(Item::Builtin(Builtin::U32)) => Ok(Type::U32),
        }
    }

    pub(crate) fn resolve_type(&self, search: &SearchPath<'_, 'ast>, ast_ty: &ast::Type<'ast>) -> CompileResult<Type<'ast>> {
        self.resolve_name_as_type(search, ast_ty.name)
    }

    pub(crate) fn lower_type(&self, ty: &Type<'ast>) -> ex::Type {
        match ty {
            Type::Unit => ex::Type::Unit,
            Type::Bool => ex::Type::Bool,
            Type::U32 => ex::Type::Integer,
            Type::Struct(struct_) => todo!(),
        }
    }
}


pub(crate) fn collect_names(ctx: LoweringContext<()>) -> CompileResult<LoweringContext<CollectedNames>> {
    let mut prelude_ns = Namespace::new();
    prelude_ns.add(ctx.builtins().kw_u32, Item::Builtin(Builtin::U32)).unwrap();

    let mut file_namespaces: HashMap<&[Symbol], Namespace> = HashMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
        let mut file_ns = Namespace::new();

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
        file_namespaces.insert(path_in_module, file_ns);
    }

    Ok(ctx.add_collected_names(CollectedNamesData { file_namespaces, prelude_ns }))
}
