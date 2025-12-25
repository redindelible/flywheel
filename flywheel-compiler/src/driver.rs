use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

use camino::{Utf8Path, Utf8PathBuf};
use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_parser::parse_source;
use flywheel_sources::{Interner, InternerState, SourceMap, Symbol};
use rayon_core::{Scope, ThreadPool, ThreadPoolBuilder};

use crate::object_pool::ObjectPool;

struct Module {
    path: Utf8PathBuf,
    ast: OnceLock<ast::Module>,
}

pub struct Driver {
    runtime: ThreadPool,
    interner_state: InternerState,
    interners: ObjectPool<Interner>,
    sources: Arc<SourceMap>,
    modules: dashmap::DashMap<String, Arc<Module>>,
}

impl Driver {
    pub fn new() -> Self {
        let runtime = ThreadPoolBuilder::new().build().unwrap();
        let sources = Arc::new(SourceMap::new());

        let interner_state = InternerState::new(Arc::clone(&sources));

        Driver {
            interners: ObjectPool::new(runtime.current_num_threads(), || interner_state.interner()),
            runtime,
            interner_state,
            sources,
            modules: dashmap::DashMap::new(),
        }
    }

    pub fn register_module(&self, name: String, path: impl Into<Utf8PathBuf>) -> CompileResult<()> {
        match self.modules.entry(name) {
            dashmap::Entry::Occupied(entry) => {
                let message = format!("there is already a registered module named {}", entry.key());
                Err(Box::new(CompileMessage::error(message)))
            }
            dashmap::Entry::Vacant(entry) => {
                entry.insert(Arc::new(Module { path: path.into(), ast: OnceLock::new() }));
                Ok(())
            }
        }
    }

    fn load_module(&self, name: String) -> CompileResult<()> {
        let module_entry = Arc::clone(&self.modules.get(&name).unwrap());

        let module = ModuleLoader::load(&self.runtime, &module_entry.path, &self.sources, &self.interners)?;
        assert!(module_entry.ast.set(module).is_ok());
        Ok(())
    }
}

struct ModuleLoader<'a> {
    root: &'a Utf8Path,
    sources: &'a SourceMap,
    interners: &'a ObjectPool<Interner>,

    contents: Mutex<HashMap<Vec<Symbol>, oneshot::Receiver<ast::File>>>,
    errors: Mutex<Vec<Box<CompileMessage>>>,
}

impl ModuleLoader<'_> {
    fn load(
        runtime: &ThreadPool,
        root: &Utf8Path,
        sources: &SourceMap,
        interners: &ObjectPool<Interner>,
    ) -> CompileResult<ast::Module> {
        let this = ModuleLoader {
            root,
            sources,
            interners,
            contents: Mutex::new(HashMap::new()),
            errors: Mutex::new(Vec::new()),
        };

        runtime.in_place_scope(|scope| {
            this.load_file(scope, vec![]);
        });

        let mut errors = this.errors.into_inner().unwrap();
        if errors.is_empty() {
            let loaded = this.contents.into_inner().unwrap();
            let mut contents = HashMap::new();
            for (path_in_module, file) in loaded {
                contents.insert(path_in_module, file.recv().unwrap());
            }
            Ok(ast::Module { contents })
        } else if errors.len() == 1 {
            Err(errors.pop().unwrap())
        } else {
            todo!()
        }
    }

    fn load_file<'a>(&'a self, scope: &Scope<'a>, path_in_module: Vec<Symbol>) {
        use std::collections::hash_map::Entry;

        let sender = match self.contents.lock().unwrap().entry(path_in_module.clone()) {
            Entry::Vacant(vacant) => {
                let (sender, receiver) = oneshot::channel();
                vacant.insert(receiver);
                sender
            }
            Entry::Occupied(_) => return,
        };

        scope.spawn(move |scope| {
            let mut path = self.root.to_owned();
            for segment in &path_in_module {
                path.push(self.sources.get_span(segment.span()));
            }
            if path.is_dir() {
                path.push("main.fly");
            } else {
                path.set_extension("fly");
            }

            let result = match std::fs::read_to_string(&path) {
                Ok(text) => {
                    let source = self.sources.add_file(path, text);
                    self.interners.with(|interner| parse_source(source, interner))
                }
                Err(_) => {
                    let message = format!("could not read from {}", &path);
                    Err(Box::new(CompileMessage::error(message)))
                }
            };

            match result {
                Ok(file) => {
                    for top_level in file.top_levels() {
                        if let ast::TopLevel::Import(import) = *top_level {
                            if import.anchor.is_none() {
                                self.load_file(scope, import.path.to_vec());
                            } else {
                                todo!()
                            }
                        }
                    }

                    sender.send(file).unwrap();
                }
                Err(error) => {
                    self.errors.lock().unwrap().push(error);
                }
            };
        })
    }
}
