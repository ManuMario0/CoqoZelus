use std::fmt::format;

use lrlex::CTLexer;
use lrpar::action_generictree;

use crate::compiler::astlustre::{Binop, Typ, Unop};

// the C program
#[derive(Debug)]
pub struct CProg {
    pub state: CState,
    pub inputs: Vec<CVar>,
    pub outputs: Vec<CVar>,
    pub local_vars: Vec<CVar>,
    pub step: CStep,
}

// A state is made up of variables
#[derive(Clone, Debug)]
pub struct CState {
    pub vars: Vec<CVar>,
}

// A step of the state is a function that takes its inputs
// executes its body and returns the next state
#[derive(Debug)]
pub struct CStep {
    pub body: Vec<Cinstruction>,
}

#[derive(Debug)]
pub enum Cinstruction {
    Cassign(CVar, Cexpr),
    Ccase(CVar, Box<Cinstruction>, Box<Cinstruction>),
}

// expressions
// CASE(var, true case, false case)
#[derive(Debug)]
pub enum Cexpr {
    Cconst(CConst),
    Cvar(CVar),
    //Clocvar(CLocalVar),
    Cbinop(Binop, Box<Cexpr>, Box<Cexpr>),
    Cunop(Unop, Box<Cexpr>),
    Cwhen(Box<Cexpr>, CVar),
}

// variables
// their depth is the depth of their history
// their role is input, output or localvar
#[derive(Clone, Debug)]
pub struct CVar {
    pub name: String,
    pub id: usize,
    pub vtype: Typ,
    pub depth: i32,
    pub role: CVarRole,
    pub init_value: Option<CConst>,
}

// A Variable can be an input, output or local variable of a program
#[derive(Clone, Debug)]
pub enum CVarRole {
    Input,
    Output,
    LocalVar,
}

// constants
#[derive(Clone, Debug)]
pub enum CConst {
    CCint(i32),
    CCbool(bool),
    CCfloat(f32),
}

pub fn generate_c_code(ast: CProg) -> String {
    /*
    first we generate the structure holding the state
    TODO: check that we have to merge state and localvars later
    */
    let state_struct = generate_state(ast.state.clone(), ast.local_vars);

    // generate init function
    let init = format!(
        "void init(State *__state_0) {{\n{}}}\n\n",
        generate_init(ast.state)
    );

    // then we generate the body of the function
    let body = generate_body(ast.step);

    // now we just generate the outline of the step function
    // we will for now assume
    let r = ast.outputs[0].clone();
    let mut prefix = format!("{} step(", generate_type(r.vtype));
    for i in ast.inputs {
        prefix = format!("{prefix} {} {}, ", generate_type(i.vtype), i.name);
    }
    let r_name = r.name;
    prefix = format!("{prefix} State *__state_0) {{\n");

    let suffix = format!("\treturn __state_0->{r_name};\n}}");

    let macros = "#include<stlib.h>\n#include<stdbool.h>\n\n\n".to_string();
    [macros, state_struct, init, prefix, body, suffix].concat()
}

fn generate_init(s: CState) -> String {
    let mut res = "".to_string();
    for v in s.vars {
        match v.init_value {
            Some(CConst::CCbool(b)) => res = format!("{res}\t__state_0->{} = {b}\n", v.name),
            Some(CConst::CCint(i)) => res = format!("{res}\t__state_0->{} = {i}\n", v.name),
            Some(CConst::CCfloat(f)) => res = format!("{res}\t__state_0->{} = {f}\n", v.name),
            None => (),
        }
    }
    res
}

fn generate_body(step: CStep) -> String {
    let mut res = "".to_string();
    for v in step.body {
        res = format!("{res}\t{};\n", generate_instruction(v))
    }
    res
}

fn generate_instruction(i: Cinstruction) -> String {
    match i {
        Cinstruction::Cassign(cvar, cexpr) => {
            let expr = generate_expr(cexpr);
            let var = cvar.name;
            format!("__state_0->{var} = {expr}")
        }
        Cinstruction::Ccase(cvar, cinstruction, cinstruction1) => {
            let expr1 = generate_instruction(*cinstruction);
            let expr2 = generate_instruction(*cinstruction1);
            let var = cvar.name;
            format!("if (__state_0->{var}) {{\n{expr1}}} else {{\n{expr2}}}")
        }
    }
}

fn generate_expr(expr: Cexpr) -> String {
    match expr {
        Cexpr::Cconst(cconst) => match cconst {
            CConst::CCint(i) => format!("{i}"),
            CConst::CCbool(b) => {
                if b {
                    format!("true")
                } else {
                    format!("false")
                }
            }
            CConst::CCfloat(f) => format!("{f}"),
        },
        Cexpr::Cvar(caccess_var) => {
            format!("__state_0->{}", caccess_var.name)
        }
        Cexpr::Cbinop(binop, cexpr, cexpr1) => {
            format!(
                "({} {} {})",
                generate_expr(*cexpr1),
                generate_binop(binop),
                generate_expr(*cexpr)
            )
        }
        Cexpr::Cunop(unop, cexpr) => {
            format!("({} {})", generate_unop(unop), generate_expr(*cexpr))
        }
        Cexpr::Cwhen(cexpr, cvar) => {
            format!("if ({}) {{\n {}}}", cvar.name, generate_expr(*cexpr))
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
        Binop::Bxor => "^", //bitwise xor but in theory should work like a charm
        Binop::Bimpl => "|| !", //or not should do the trick
    }
    .to_string()
}

fn generate_unop(u: Unop) -> String {
    match u {
        Unop::Uneg => "-",
        Unop::Unot => "!",
    }
    .to_string()
}

fn generate_state(s: CState, m: Vec<CVar>) -> String {
    let mut res = "typedef struct {".to_string();
    for v in s.vars {
        res = format!("{res}\n\t {} {};", generate_type(v.vtype), v.name)
    }
    for v in m {
        res = format!("{res}\n\t {} {};", generate_type(v.vtype), v.name)
    }
    res = format!("{res}\n}} State;\n\n");
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
