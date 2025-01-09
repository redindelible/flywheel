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
    let maybe_ast = frontend.block_on(handle.query_declared_types(source));
    dbg!(start.elapsed());
    match maybe_ast {
        Ok(_) => {
            print!("got collected");
        },
        Err(e) => {
            print!("{}", e.display(&handle));
        }
    }
}
