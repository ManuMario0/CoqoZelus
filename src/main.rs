use parser::FileObj;
use typing::flatten_const;

pub mod parser;
mod typing;

fn main() {
    match FileObj::new(
        "/Users/emmanuel/Documents/developement/shared-projects/reactive-sys/CoqoZelus/tests/test",
    ) {
        Ok(obj) => {
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
