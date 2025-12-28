use std::collections::HashMap;
use std::sync::Arc;
use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::{SourceMap, Span, Symbol};

enum Item<'ast> {
    Imported(&'ast ast::Import<'ast>),
    Function(&'ast ast::Function<'ast>),
    Struct(&'ast ast::Struct<'ast>),
}

impl<'ast> Item<'ast> {
    fn span(&self) -> Span {
        match self {
            Item::Imported(item) => item.span,
            Item::Function(item) => item.span,
            Item::Struct(item) => item.span,
        }
    }
}

struct Namespace<'ast> {
    sources: Arc<SourceMap>,
    items: HashMap<Symbol, Item<'ast>>
}

impl<'ast> Namespace<'ast> {
    fn add(&mut self, name: Symbol, item: Item<'ast>) -> CompileResult<()> {
        use std::collections::hash_map::Entry;

        match self.items.entry(name) {
            Entry::Vacant(vacant) => {
                vacant.insert(item);
            },
            Entry::Occupied(occupied) => {
                let name = self.sources.get_span(name.span());
                let message = CompileMessage::error(format!("There's already a thing called {}", name))
                    .with_span(item.span())
                    .with_child(CompileMessage::note("Previously declared here")
                        .with_span(occupied.get().span())
                    );
                return Err(Box::new(message));
            }
        }
        Ok(())
    }
}


pub fn lower(ast: &ast::Module) -> CompileResult<()> {
    let mut file_namespaces: HashMap<&[Symbol], Namespace> = HashMap::new();
    for (path_in_module, file) in &ast.contents {
        let mut namespace = Namespace { sources: Arc::clone(&ast.sources), items: HashMap::new() };

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

    // for (path_in_module, file) in &ast.contents {
    //     for top_level in file.top_levels() {
    //         match *top_level {
    //             ast::TopLevel::Struct(struct_) => {
    //                 struct_.
    //             }
    //         }
    //     }
    // }

    Ok(())
}
