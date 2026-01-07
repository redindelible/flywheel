mod namespace;

use std::collections::HashMap;
use std::sync::Arc;

use by_address::ByAddress;
use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::Symbol;

use crate::namespace::{Builtin, Item, Namespace};

enum Type<'ast> {
    U32,
    Struct(&'ast ast::Struct<'ast>),
}

fn resolve_name_as_type<'ast, 'ns>(
    namespace: &Namespace<'ast, 'ns>,
    file_namespaces: &HashMap<&[Symbol], Namespace<'ast, 'ns>>,
    name: Symbol,
) -> CompileResult<Type<'ast>> {
    match namespace.resolve(name) {
        None => {
            let message = format!("No type found called {}", namespace.resolve_symbol(name));
            Err(Box::new(CompileMessage::error(message)))
        }
        Some(Item::Function(_)) => {
            let message = format!("No type found called {}", namespace.resolve_symbol(name));
            Err(Box::new(CompileMessage::error(message)))
        }
        Some(Item::Imported(import)) => {
            assert!(import.anchor.is_none());
            let namespace = &file_namespaces[import.path];
            resolve_name_as_type(namespace, file_namespaces, name) // todo cache this and prevent cycles
        }
        Some(Item::Struct(struct_)) => Ok(Type::Struct(struct_)),
        Some(Item::Builtin(Builtin::U32)) => Ok(Type::U32),
    }
}

fn resolve_type<'ast, 'ns>(
    namespace: &Namespace<'ast, 'ns>,
    file_namespaces: &HashMap<&[Symbol], Namespace<'ast, 'ns>>,
    ast_ty: &ast::Type<'ast>,
) -> CompileResult<Type<'ast>> {
    resolve_name_as_type(namespace, file_namespaces, ast_ty.name)
}

pub const BUILTINS: &[&str] = &["u32"];

pub fn lower(ast: &ast::Module, builtins: &HashMap<&'static str, Symbol>) -> CompileResult<()> {
    let mut prelude_namespace = Namespace::new(Arc::clone(&ast.sources), None);
    prelude_namespace.add(builtins["u32"], Item::Builtin(Builtin::U32)).unwrap();

    let mut file_namespaces: HashMap<&[Symbol], Namespace> = HashMap::new();
    for (path_in_module, file) in &ast.contents {
        let mut namespace = Namespace::new(Arc::clone(&ast.sources), Some(&prelude_namespace));

        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Import(import) => {
                    namespace.add(import.item, Item::Imported(import))?;
                }
                ast::TopLevel::Function(func) => {
                    namespace.add(func.name, Item::Function(func))?;
                }
                ast::TopLevel::Struct(struct_) => {
                    namespace.add(struct_.name, Item::Struct(struct_))?;
                }
            }
        }
        file_namespaces.insert(path_in_module, namespace);
    }

    type StructFields<'ast> = HashMap<Symbol, Type<'ast>>;
    let mut struct_fields: HashMap<ByAddress<&ast::Struct>, StructFields> = HashMap::new();

    struct FunctionSignature<'ast> {
        return_type: Type<'ast>,
    }
    let mut function_signatures: HashMap<ByAddress<&ast::Function>, FunctionSignature> = HashMap::new();

    for (path_in_module, file) in &ast.contents {
        let file_namespace = &file_namespaces[path_in_module.as_slice()];
        for top_level in file.top_levels() {
            match *top_level {
                ast::TopLevel::Struct(struct_) => {
                    let mut fields: StructFields = HashMap::new();

                    for field in struct_.fields {
                        let ty = resolve_type(file_namespace, &file_namespaces, &field.ty)?;
                        fields.insert(field.name, ty);
                    }
                    assert!(struct_fields.insert(ByAddress(struct_), fields).is_none());
                }
                ast::TopLevel::Function(function) => {
                    let return_type = resolve_type(file_namespace, &file_namespaces, &function.return_type)?;
                    let signature = FunctionSignature { return_type };
                    assert!(function_signatures.insert(ByAddress(function), signature).is_none());
                }
                ast::TopLevel::Import(_) => (),
            };
        }
    }

    Ok(())
}
