use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::Symbol;

use crate::context::{CollectedNames, CollectedNamesData, LoweringContext};
use crate::namespace::{Builtin, Item, Namespace, SearchPath, Value};
use crate::types::Type;

impl<'ast> LoweringContext<'ast, CollectedNames> {
    pub fn resolve_name_as_type<'a>(&self, search_path: impl SearchPath<'a, 'ast>, name: ast::Name) -> CompileResult<Type<'ast>> where 'ast: 'a {
        let (found_name, span) = match Namespace::search(search_path, name)? {
            Item::Function(func) => {
                ("function", func.span)
            }
            Item::Local { span, .. } => {
                ("local", *span)
            }
            Item::Imported(import) => {
                assert!(import.anchor.is_none());
                return self.resolve_name_as_type([&self.collected_names().file_namespaces[import.path]], name)
                    .map_err(|msg|
                        msg.with_child(
                            CompileMessage::note_dyn(move |s| format!("`{}` was imported here", s.get_symbol(name.symbol)))
                                .with_span(import.span))
                    );
                // todo cache this and prevent cycles
            }
            Item::Struct(struct_) => return Ok(Type::Struct(struct_)),
            Item::Builtin(Builtin::U32) => return Ok(Type::U32),
        };

        Err(CompileMessage::error_dyn(move |s| format!("Expected type, found {} `{}`", found_name, s.get_symbol(name.symbol))).with_span(name.span).with_child(CompileMessage::note("Defined here").with_span(span)))
    }

    pub fn resolve_name_as_value<'a>(&self, search_path: impl SearchPath<'a, 'ast>, name: ast::Name) -> CompileResult<Value<'ast>> where 'ast: 'a {
        let (found_name, span) = match Namespace::search(search_path, name)? {
            Item::Function(func) => {
                todo!()
            }
            Item::Local { index, ty, span } => {
                return Ok(Value::Local { index: *index, ty: ty.clone(), span: *span });
            }
            Item::Imported(import) => {
                assert!(import.anchor.is_none());
                return self.resolve_name_as_value([&self.collected_names().file_namespaces[import.path]], name)
                    .map_err(|msg|
                        msg.with_child(
                            CompileMessage::note_dyn(move |s| format!("`{}` was imported here", s.get_symbol(name.symbol)))
                                .with_span(import.span))
                    );
                // todo cache this and prevent cycles
            }
            Item::Struct(struct_) => ("struct", Some(struct_.span)),
            Item::Builtin(Builtin::U32) => ("builtin u32", None),
        };

        let base_msg = CompileMessage::error_dyn(move |s| format!("Expected type, found {} `{}`", found_name, s.get_symbol(name.symbol))).with_span(name.span);
        if let Some(span) = span {
            Err(base_msg.with_child(CompileMessage::note("Defined here").with_span(span)))
        } else {
            Err(base_msg)
        }
    }

    pub(crate) fn resolve_type<'a>(&self, search: impl SearchPath<'a, 'ast>, ast_ty: &ast::Type<'ast>) -> CompileResult<Type<'ast>> where 'ast: 'a {
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
                    file_ns.add(func.name.symbol, Item::Function(func))?;
                }
                ast::TopLevel::Struct(struct_) => {
                    file_ns.add(struct_.name.symbol, Item::Struct(struct_))?;
                }
            }
        }
        file_namespaces.insert(path_in_module, file_ns);
    }

    Ok(ctx.add_collected_names(CollectedNamesData { file_namespaces, prelude_ns }))
}
