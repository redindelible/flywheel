use std::time::Instant;
use crate::frontend::FrontendDriver;

// mod codegen;
// mod interpreter;
mod utils;
mod frontend;

fn main() {
    let frontend = FrontendDriver::new();
    let handle = frontend.handle();
    let source = frontend.block_on(handle.query_file_source("test/import.fly".into())).unwrap();
    
    let start = Instant::now();
    let maybe_ast = frontend.block_on(handle.spawn(handle.query_ast(source))).unwrap();
    dbg!(start.elapsed());
    match maybe_ast {
        Ok(ast) => {
            print!("{}", ast.pretty(2));
        },
        Err(e) => {
            print!("{}", e.display(&handle));
        }
    }
}
