use std::fmt::format;

use lrlex::CTLexer;
use lrpar::action_generictree;

// binary operations (excepted fby and arrow)
pub enum Binop {
    Badd,
    Bsub,
    Bmul,
    Bdiv,
    Bleq,
    Bgeq,
    Bl,
    Bg,
    Beq,
    Bneq,
    Band,
    Bor,
    Bxor,
    Bimpl,
}

// unary operations (excepted pre)
pub enum Unop {
    Uneg,
    Unot,
}



#[derive(Clone)]
// types
pub enum Typ {
    Tint,
    Tbool,
    Treal,
    Tvec(u64, Box<Typ>)
}

// the C program
// state is the struct
// memory are the local variables made during normalization
// step is the function step
// I WILL REMOVE INPUTS AND OUTPUTS IN FINAL VERSION
pub struct CProg {
    pub state: CState,
    pub memory : CMemory,
    pub inputs : Vec<CVar>,
    pub outputs : Vec<CVar>,
    pub step: CStep,
}

#[derive(Clone)]
pub struct CMemory {
    pub vars: Vec<CLocalVar>
}

// A state is made up of variables
#[derive(Clone)]
pub struct CState {
    pub vars: Vec<CVar>,
}

// A step of the state is a function that takes its inputs
// executes its body and returns the next state
pub struct CStep {
    //inputs : Vec<CVar>,
    pub body: Vec<Cexpr>,
}

// expressions
// CASE(var, true case, false case)
pub enum Cexpr {
    Cconst(CConst),
    Cvar(CAccessVar),
    Clocvar(CLocalVar),
    Cassign(CVar, Box<Cexpr>),
    Cbinop(Binop, Box<Cexpr>, Box<Cexpr>),
    Cunop(Unop, Box<Cexpr>),
    Cwhen(Box<Cexpr>, CVar),
    Ccase(CVar, Box<Cexpr>, Box<Cexpr>),
}

// access to a variable in a program
// contains the additional info of which depth needs to be accessed
pub struct CAccessVar {
    var: CVar,
    depth: i32,
}

// variables
// their depth is the depth of their history
// their role is input, output or localvar
#[derive(Clone)]
pub struct CVar {
    pub name: String,
    pub id: usize,
    pub vtype: Typ,
    pub depth: i32,
    pub role: CVarRole,
    pub init_value: Option<CConst>,
}

#[derive(Clone)]
pub struct CLocalVar {
    pub name: String,
    pub id : usize,
    pub vtype : Typ,
    pub init_value: Option<CConst>,
}

// A Variable can be an input, output or local variable of a program
#[derive(Clone)]
pub enum CVarRole {
    Input,
    Output,
    LocalVar,
}

// constants
#[derive(Clone)]
pub enum CConst {
    CCint(i32),
    CCbool(bool),
    CCfloat(f32),
}


pub fn generate_c_code(ast: CProg) -> String {
    // first we generate the structure holding the state
    let state_struct = generate_state(ast.state, ast.memory);

    // then we generate the body of the function
    let body = generate_body(ast.step);

    // now we just generate the outline of the step function
    // we will for now assume
    let r = ast.outputs[0].clone();
    let mut prefix = format!("{} step(", generate_type(r.vtype));
    for i in ast.inputs {
        prefix = format!("{prefix} {} {}, ", generate_type(i.vtype), i.name);
    }
    prefix = format!("{prefix} State *__state_0) {{");

    let suffix = "}".to_string();

    let macros = "#include<stlib.h>\n#include<stdbool.h>\n\n\n".to_string();
    [macros, state_struct, prefix, body, suffix].concat()
}

fn generate_body(step: CStep) -> String {
    let mut res = "".to_string();
    for v in step.body {
        res = format!("{res} {}", generate_expr(v))
    }
    res
}

fn generate_expr(expr: Cexpr) -> String {
    match expr {
        Cexpr::Cconst(cconst) => {
            match cconst {
                CConst::CCint(i) => format!("{i}"),
                CConst::CCbool(b) => {
                    if b {
                        format!("true")
                    } else {
                        format!("false")
                    }
                },
                CConst::CCfloat(f) => format!("{f}"),
            }
        },
        Cexpr::Cvar(caccess_var) => {
            format!("__state_0->{}", caccess_var.var.name)
        },
        Cexpr::Clocvar(cloc_var) => {
            format!("{}", cloc_var.name)
        },
        Cexpr::Cassign(cvar, cexpr) => {
            format!("\t{} = {};\n", cvar.name, generate_expr(*cexpr))
        }
        Cexpr::Cbinop(binop, cexpr, cexpr1) => {
            format!("({} {} {})", generate_expr(*cexpr1), generate_binop(binop), generate_expr(*cexpr))
        }
        Cexpr::Cunop(unop, cexpr) => {
            format!("({} {})", generate_unop(unop), generate_expr(*cexpr))
        }
        Cexpr::Cwhen(cexpr, cvar) => {
            format!("if ({}) {{\n {}}}", cvar.name, generate_expr(*cexpr))
        }
        Cexpr::Ccase(cvar, cexpr, cexpr1) => {
            format!("if ({}) {{\n {}}} else {{\n {}}}", cvar.name, generate_expr(*cexpr), generate_expr(*cexpr1))
        }
    }
}

// we permute all operations only for the impl, which requires the first operand on the right
fn generate_binop(b: Binop) -> String {
    match b {
        Binop::Badd => "+",
        Binop::Bsub => "-",
        Binop::Bmul => "*",
        Binop::Bdiv => "/",
        Binop::Bleq => ">=",
        Binop::Bgeq => "<=",
        Binop::Bl => ">",
        Binop::Bg => "<",
        Binop::Beq => "==",
        Binop::Bneq => "!=",
        Binop::Band => "&&",
        Binop::Bor => "||",
        Binop::Bxor => "^",//bitwise xor but in theory should work like a charm
        Binop::Bimpl => "|| !",//or not should do the trick
    }.to_string()
}

fn generate_unop(u: Unop) -> String {
    match u {
        Unop::Uneg => "-",
        Unop::Unot => "!",
    }.to_string()
}

fn generate_state(s: CState, m: CMemory) -> String {
    let mut res = "struct State {".to_string();
    for v in s.vars {
        res = format!("{res}\n\t {} {};", generate_type(v.vtype), v.name)
    }
    for v in m.vars {
        res = format!("{res}\n\t {} {};", generate_type(v.vtype), v.name)
    }
    res = format!("{res}\n}};\n");
    res
}

fn generate_type(t: Typ) -> String {
    match t {
        Typ::Tint => "int".to_string(),
        Typ::Tbool => "bool".to_string(),
        Typ::Treal => "double".to_string(),
        Typ::Tvec(_, typ) => todo!(),
    }
}