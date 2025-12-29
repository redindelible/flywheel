use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

use camino::{Utf8Path, Utf8PathBuf};
use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_lower::lower;
use flywheel_parser::parse_source;
use flywheel_sources::{Interner, InternerState, SourceMap, Symbol};
use rayon::{ThreadPool, ThreadPoolBuilder};

use crate::object_pool::ObjectPool;

pub struct Driver {
    runtime: ThreadPool,
    interner_state: InternerState,
    interners: Arc<ObjectPool<Interner>>,
    sources: Arc<SourceMap>,
    modules: dashmap::DashMap<String, Arc<OnceLock<()>>>,
}

impl Driver {
    pub fn new() -> Self {
        let runtime = ThreadPoolBuilder::new().build().unwrap();
        let sources = Arc::new(SourceMap::new());

        let interner_state = InternerState::new(Arc::clone(&sources));
        let interners = Arc::new(ObjectPool::new(runtime.current_num_threads(), || interner_state.interner()));

        Driver { interners, runtime, interner_state, sources, modules: dashmap::DashMap::new() }
    }

    pub fn add_module(&self, name: impl Into<String>, path: impl Into<Utf8PathBuf>) -> CompileResult<()> {
        let item = match self.modules.entry(name.into()) {
            dashmap::Entry::Occupied(entry) => {
                let message = format!("there is already a registered module named {}", entry.key());
                return Err(Box::new(CompileMessage::error(message)));
            }
            dashmap::Entry::Vacant(entry) => Arc::clone(&entry.insert(Arc::new(OnceLock::new()))),
        };

        let sources = Arc::clone(&self.sources);
        let interners = Arc::clone(&self.interners);
        let path = path.into();
        let module = self.runtime.install(move || ModuleLoader::load(path, sources, interners))?;
        lower(&module)?;

        assert!(item.set(()).is_ok());
        Ok(())
    }
}

struct ModuleLoader {
    root: Utf8PathBuf,
    sources: Arc<SourceMap>,
    interners: Arc<ObjectPool<Interner>>,

    contents: Mutex<HashMap<Vec<Symbol>, oneshot::Receiver<ast::File>>>,
    errors: Mutex<Vec<Box<CompileMessage>>>,
}

impl ModuleLoader {
    fn load(
        root: impl Into<Utf8PathBuf>,
        sources: Arc<SourceMap>,
        interners: Arc<ObjectPool<Interner>>,
    ) -> CompileResult<ast::Module> {
        let this = ModuleLoader {
            root: root.into(),
            sources,
            interners,
            contents: Mutex::new(HashMap::new()),
            errors: Mutex::new(Vec::new()),
        };

        rayon::scope(|scope| {
            this.load_file(scope, vec![]);
        });

        let mut errors = this.errors.into_inner().unwrap();
        if errors.is_empty() {
            let loaded = this.contents.into_inner().unwrap();
            let mut contents = HashMap::new();
            for (path_in_module, file) in loaded {
                contents.insert(path_in_module, file.recv().unwrap());
            }
            Ok(ast::Module { sources: this.sources, contents })
        } else if errors.len() == 1 {
            Err(errors.pop().unwrap())
        } else {
            todo!()
        }
    }

    fn load_file<'a>(&'a self, scope: &rayon::Scope<'a>, path_in_module: Vec<Symbol>) {
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
                path.push(self.sources.get_symbol(*segment));
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

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use flywheel_sources::{InternerState, SourceMap};

    use crate::driver::ModuleLoader;
    use crate::object_pool::ObjectPool;

    fn check_module_loader(root: &str, mut expected_paths: Vec<&[&str]>) {
        let sources = Arc::new(SourceMap::new());
        let interner_state = InternerState::new(Arc::clone(&sources));
        let interners = Arc::new(ObjectPool::new(16, || interner_state.interner()));

        let module = ModuleLoader::load(root, sources, interners).unwrap();
        let mut actual_paths: Vec<Vec<String>> = Vec::new();
        for symbol_path in module.contents.keys() {
            let resolved_path: Vec<String> =
                symbol_path.iter().map(|&symbol| interner_state.resolve(symbol).to_string()).collect();
            actual_paths.push(resolved_path);
        }
        actual_paths.sort();
        expected_paths.sort();
        assert_eq!(actual_paths, expected_paths);
    }

    #[test]
    fn test_load_single_file() {
        check_module_loader("tests/single_file", vec![&[]]);
    }

    #[test]
    fn test_load_one_imported() {
        check_module_loader("tests/one_imported", vec![&[], &["imported"]]);
    }
}
