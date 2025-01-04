use std::time::Instant;
use crate::frontend::FrontendDriver;

// mod codegen;
// mod interpreter;
mod utils;
mod frontend;

fn main() {
    let frontend = FrontendDriver::new();
    let source = frontend.block_on(frontend.query_file_source("test/import.fly")).unwrap();
    
    let start = Instant::now();
    let maybe_ast = frontend.block_on(frontend.query_ast(source));
    dbg!(start.elapsed());
    match maybe_ast {
        Ok(ast) => {
            print!("{}", ast.pretty(2));
        },
        Err(e) => {
            print!("{}", e.display(&frontend));
        }
    }
}
