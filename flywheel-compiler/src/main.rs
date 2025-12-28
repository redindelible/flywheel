use std::time::Instant;

use flywheel_compiler::Driver;

fn main() {
    let driver = Driver::new();
    driver.add_module("<main>", "flywheel-parser/test/simple.fly").unwrap();
    // let source = frontend.block_on(frontend.query_file_source("test/import.fly")).unwrap();
    //
    // let start = Instant::now();
    // let maybe_ast = frontend.block_on(frontend.query_defined_types(source));
    // dbg!(start.elapsed());
    // match maybe_ast {
    //     Ok(_) => {
    //         print!("got collected");
    //     }
    //     Err(e) => {
    //         print!("{}", e.display(&frontend));
    //     }
    // }
}
