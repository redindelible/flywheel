use futures_util::{FutureExt, StreamExt};
use crate::utils::InternedString;
use super::ast::AstRef;
use super::{ast, CompileResult, Handle, SourceID};

#[derive(Copy, Clone)]
enum Name {
    Module(SourceID),
    Struct(SourceID, AstRef<ast::Struct>)
}

pub struct DeclaredTypes {
    file_namespace: im::HashMap<InternedString, Name>
}

impl DeclaredTypes {
    pub async fn process(handle: Handle, source_id: SourceID) -> CompileResult<Self> {
        let strings = handle.strings();
        let ast = handle.query_ast(source_id).await?;
        let handle_ref = &handle;

        let mut imports_stream = ast.top_levels()
            .iter()
            .filter_map(|top_level| {
                match top_level {
                    &ast::TopLevel::Import(ast_import) => {
                        let relative_path = camino::Utf8PathBuf::from(strings.resolve(ast.get_node(ast_import).relative_path).unwrap().to_owned());
                        Some(handle_ref.query_relative_source(source_id, relative_path.clone()).map(|source| (relative_path, source)))
                    }
                    _ => None
                }
            })
            .collect::<futures_util::stream::FuturesOrdered<_>>();

        let mut file_namespace = im::HashMap::new();
        for top_level in ast.top_levels() {
            match top_level {
                &ast::TopLevel::Import(_) => {
                    let (relative_path, maybe_source) = imports_stream.next().await.unwrap();
                    let source = maybe_source?;
                    let name = relative_path.file_name().unwrap_or_else(|| todo!()).strip_suffix(".fly").unwrap_or_else(|| todo!());
                    let name = strings.get_or_intern(name);
                    file_namespace.insert(name, Name::Module(source));
                }
                &ast::TopLevel::Struct(ast_struct) => {
                    let struct_ = ast.get_node(ast_struct);
                    file_namespace.insert(struct_.name, Name::Struct(source_id, ast_struct));
                }
                &ast::TopLevel::Function(_) => ()
            }
        }

        Ok(DeclaredTypes {
            file_namespace
        })
    }
}
