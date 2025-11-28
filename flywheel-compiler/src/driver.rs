use std::collections::HashMap;
use std::sync::Arc;

use camino::Utf8Path;
// use rayon_core::{ThreadPool, ThreadPoolBuilder};

use flywheel_error::{CompileMessage, CompileResult};
use flywheel_parser::parse_source;
use flywheel_ast as ast;
use flywheel_sources::{InternerState, SourceMap, Symbol};

pub struct Driver {
    // runtime: ThreadPool,
    interner_state: InternerState,
    sources: Arc<SourceMap>,
}

impl Driver {
    pub fn new() -> Self {
        // let runtime = ThreadPoolBuilder::new().build().unwrap();
        let sources = Arc::new(SourceMap::new());

        Driver {
            // runtime,
            interner_state: InternerState::new(Arc::clone(&sources)),
            sources,
        }
    }

    pub fn load_module(&mut self, path: impl AsRef<Utf8Path>) -> CompileResult<()> {
        let mut interner = self.interner_state.interner();

        let mut queue = vec![(vec![], path.as_ref().join("main.fly"))];
        let mut contents: HashMap<Vec<Symbol>, ast::File> = HashMap::new();

        while let Some((module_relative_path, path)) = queue.pop() {
            let Ok(text) = std::fs::read_to_string(&path) else {
                let message = format!("could not read from {}", &path);
                return Err(Box::new(CompileMessage::error(message)));
            };
            let source = self.sources.add_file(path, text);
            let file_ast = parse_source(source, &mut interner)?;
            contents.insert(module_relative_path, file_ast);
        }
        
        let _ = ast::Module { contents };
        Ok(())
    }
}
