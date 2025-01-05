use parser::FileObj;
use typing::{flatten_const, simplify_ast, type_ast};

pub mod parser;
mod typing;
mod transpile;
mod compiler;

fn main() {
    match FileObj::new(
        "/Users/emmanuel/Documents/developement/shared-projects/reactive-sys/CoqoZelus/tests/test",
    ) {
        Ok(obj) => {
            println!("{:?}", obj.get_ast());
            let obj = simplify_ast(obj).unwrap();
            println!("{:?}", obj.get_ast());
            let obj =  type_ast(obj).unwrap();
            println!("{:?}", obj.get_ast());
            println!("{:?}", flatten_const(obj).unwrap().get_ast());
            assert!(true);
        }
        Err(e) => {
            println!("{}", e);
            assert!(false);
        }
    }
}
