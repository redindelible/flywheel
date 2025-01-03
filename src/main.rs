use std::time::Instant;
use crate::frontend::FrontendDriver;

// mod codegen;
// mod interpreter;
mod utils;
mod frontend;

fn main() {
    let frontend = FrontendDriver::new();
    let source = frontend.add_file_source("test/import.fly").unwrap();
    
    let start = Instant::now();
    let maybe_ast = frontend.query_ast(source);
    dbg!(start.elapsed());
    match maybe_ast {
        Ok(ast) => {
            print!("{}", ast.pretty(1));
        },
        Err(e) => {
            print!("{}", e.with(&frontend));
        }
    }
}
