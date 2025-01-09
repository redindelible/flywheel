use std::collections::HashMap;
use std::sync::Arc;
use camino::Utf8Path;
use futures::stream::{StreamExt, FuturesUnordered};
use super::ast::{AstRef, FileAST, Import, Struct};
use super::{ast, CompileResult, Handle, SourceID};
use crate::utils::{declare_sharded_key, InternedString, ShardedKey};

declare_sharded_key! {
    struct NamespaceKey(SourceID, 1);
}

pub struct TypeCheckerShared {
    
}

impl TypeCheckerShared {
    pub fn new() -> TypeCheckerShared {
        TypeCheckerShared { }
    }
}

pub struct CollectImports {
    ast: Arc<FileAST>,
    imports: HashMap<AstRef<Import>, SourceID>,
    file_namespace: NamespaceKey
}

impl CollectImports {
    pub async fn process(handle: Handle, source_id: SourceID) -> CompileResult<Self> {
        let strings = handle.strings();
        let ast = handle.query_ast(source_id).await?;
        let handle_ref = &handle;
        
        let file_namespace = NamespaceKey::sentinel(ast.source(), 0);

        let mut imports_stream = ast.top_levels()
            .iter()
            .filter_map(|top_level| {
                match top_level {
                    &ast::TopLevel::Import(ast_import) => {
                        let relative_path = strings.resolve(ast.get(ast_import).relative_path).unwrap().to_owned();
                        Some(async move {
                            (ast_import, handle_ref.query_relative_source(source_id, Utf8Path::new(&relative_path)).await)
                        })
                    }
                    _ => None
                }
            })
            .collect::<FuturesUnordered<_>>();
        let mut imports = HashMap::new();
        while let Some((ast_import, maybe_source)) = imports_stream.next().await {
            imports.insert(ast_import, maybe_source?);
        }
        
        Ok(CollectImports {
            ast,
            imports,
            file_namespace
        })
    }
}

struct StructDeclaration {
    name: InternedString,
}

pub struct DeclaredTypes {
    ast: Arc<FileAST>,

    file_namespace: NamespaceKey,
}

enum Type {
    
}

struct StructDefinition {
    fields: HashMap<InternedString, Type>
}

pub struct DefinedTypes {
    ast: Arc<FileAST>,
    
    structs: HashMap<AstRef<Struct>, StructDefinition>
}