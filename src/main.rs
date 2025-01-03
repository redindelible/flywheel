use std::time::Instant;
use crate::frontend::FrontendDriver;

mod codegen;
mod interpreter;
mod utils;
mod frontend;

fn main() {
    let frontend = FrontendDriver::new();
    let source = frontend.add_string_source("simple.fly", include_str!("../test/control-flow.fly"));
    
    let start = Instant::now();
    let maybe_ast = frontend.query_ast(source.id());
    dbg!(start.elapsed());
    match maybe_ast {
        Ok(ast) => {
            print!("{}", ast.pretty(1));
        },
        Err(e) => { dbg!(e); }
    }
}
