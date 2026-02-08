mod context;
mod namespace;

use std::collections::HashMap;
use std::sync::Arc;

use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::Symbol;

use crate::context::{
    AllFunctionSignatures, AllFunctionSignaturesData, AllStructFields, AllStructFieldsData, AstMap, CollectedNames,
    CollectedNamesData, FunctionSignature, LoweringContext,
};
use crate::namespace::{Builtin, Item, Namespace};

enum Type<'ast> {
    U32,
    Struct(&'ast ast::Struct<'ast>),
}

impl<'ast> LoweringContext<'ast, CollectedNames> {
    fn resolve_name_as_type(&self, in_ns: &Namespace<'ast>, name: Symbol) -> CompileResult<Type<'ast>> {
        match in_ns.resolve(name) {
            None => Err(CompileMessage::error_dyn(move |s| format!("No type found called {}", s.get_symbol(name)))),
            Some(Item::Function(_)) => {
                Err(CompileMessage::error_dyn(move |s| format!("No type found called {}", s.get_symbol(name))))
            }
            Some(Item::Imported(import)) => {
                assert!(import.anchor.is_none());
                self.resolve_name_as_type(&self.collected_names().file_namespaces[import.path], name) // todo cache this and prevent cycles
            }
            Some(Item::Struct(struct_)) => Ok(Type::Struct(struct_)),
            Some(Item::Builtin(Builtin::U32)) => Ok(Type::U32),
        }
    }

    fn resolve_type(&self, in_ns: &Namespace<'ast>, ast_ty: &ast::Type<'ast>) -> CompileResult<Type<'ast>> {
        self.resolve_name_as_type(in_ns, ast_ty.name)
    }
}

pub const BUILTINS: &[&str] = &["u32"];

fn collect_names(ctx: LoweringContext<()>) -> CompileResult<LoweringContext<CollectedNames>> {
    let mut prelude_ns = Namespace::new_root();
    prelude_ns.add(ctx.builtins()["u32"], Item::Builtin(Builtin::U32)).unwrap();
    let prelude_ns = Arc::new(prelude_ns);

    let mut file_namespaces: HashMap<&[Symbol], Arc<Namespace>> = HashMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
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

    Ok(ctx.add_collected_names(CollectedNamesData { file_namespaces, prelude_ns }))
}

fn collect_struct_fields(ctx: LoweringContext<CollectedNames>) -> CompileResult<LoweringContext<AllStructFields>> {
    let mut all_struct_fields = AstMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
        let file_namespace = &*ctx.collected_names().file_namespaces[path_in_module.as_slice()];
        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Struct(struct_) => {
                    let mut fields = HashMap::new();

                    for field in struct_.fields {
                        let ty = ctx.resolve_type(file_namespace, &field.ty)?;
                        fields.insert(field.name, ty);
                    }
                    all_struct_fields.insert(struct_, fields);
                }
                ast::TopLevel::Function(_) => (),
                ast::TopLevel::Import(_) => (),
            };
        }
    }

    Ok(ctx.add_all_struct_fields(AllStructFieldsData { all_struct_fields }))
}

fn collect_function_signatures(
    ctx: LoweringContext<AllStructFields>,
) -> CompileResult<LoweringContext<AllFunctionSignatures>> {
    let mut signatures = AstMap::new();
    for (path_in_module, file) in &ctx.ast().contents {
        let file_namespace = &*ctx.collected_names().file_namespaces[path_in_module.as_slice()];
        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Function(func_) => {
                    let return_type = ctx.resolve_type(file_namespace, &func_.return_type)?;
                    signatures.insert(func_, FunctionSignature { return_type });
                }
                _ => (),
            };
        }
    }

    Ok(ctx.add_all_function_signatures(AllFunctionSignaturesData { signatures }))
}

pub fn lower(ast: &ast::Module, builtins: &HashMap<&'static str, Symbol>) -> CompileResult<()> {
    let ctx = LoweringContext::new(ast, builtins);
    let ctx = collect_names(ctx)?;
    let ctx = collect_struct_fields(ctx)?;
    let ctx = collect_function_signatures(ctx)?;

    let _ = ctx;

    Ok(())
}
