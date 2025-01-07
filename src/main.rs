use compiler::compile::compile;
use parser::FileObj;
use transpile::generate_c_code;
use typing::{flatten_const, simplify_ast, type_ast};

mod compiler;
pub mod parser;
mod transpile;
mod typing;

fn main() {
    match FileObj::new("tests/merge_fby") {
        Ok(obj) => {
            println!("{:?}", obj.get_ast());
            let obj = simplify_ast(obj).unwrap();
            println!("{:?}", obj.get_ast());
            let obj = type_ast(obj).unwrap();
            println!("{:?}", obj.get_ast());
            let obj = flatten_const(obj).unwrap();
            println!("{:?}", obj.get_ast());
            let ast = typing::translate(&obj).unwrap();
            println!("{:?}", ast);
            let c = compile(ast);
            println!("{:?}", c);
            let prog = generate_c_code(c);
            println!("{prog}");
            assert!(true);
        }
        Err(e) => {
            println!("{}", e);
            assert!(false);
        }
    }
}
