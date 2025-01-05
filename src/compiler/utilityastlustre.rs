
/* Utility functions for AST LUSTRE */

use core::panic;

use crate::{compiler::{astc::{self}, astlustre::{Equation, Expr, Node, Var}}, transpile::CState};

use crate::transpile::{CProg, CVar, CVarRole};

use super::astlustre::{Constant, Typ};

pub fn build_node(name : String, input : Vec<Var>, output : Vec<Var>, local_vars : Vec<Var>, body : Vec<Equation>) -> Node {
    Node { name, input, output, local_vars, body}
}

pub fn build_equation(var : Var, expression : Expr) -> Equation {
    Equation { var, expression }
}

// Checks that two variables are equal
pub fn eq_var(x: &Var, y: &Var) -> bool {
    x.id == y.id && x.name == y.name
}

// Checks if a variable is in a vector
pub fn var_in(x: &Var, vars: &Vec<Var>) -> bool {
    for v in vars {
        if eq_var(x, v) {
            return true;
        }
    }
    false
}

// Build a new local variable
pub fn build_var(name : String, id : usize, vtype : Typ) -> Var {
    Var { name, id, vtype }
}

pub fn new_localvar(id : usize, nb_equ : i32, vtype : Typ) -> Var {
    build_var(format!("local_{}_{}", nb_equ, id), id, vtype)
}

pub fn new_fbyvar(id : usize, vtype : Typ) -> Var {
    build_var(format!("fby_{}", id), id, vtype)
}

// Finds number of variables in a body
pub fn number_vars(node: &Node) -> usize {
    let mut max_id = 0;
    for var in &node.input {
        if max_id < var.id {
            max_id = var.id
        }
    }
    for var in &node.output {
        if max_id < var.id {
            max_id = var.id
        }
    }
    for var in &node.local_vars {
        if max_id < var.id {
            max_id = var.id
        }
    }
    max_id
}

// push equation
pub fn push_equ(past_equ : Vec<Equation>, v : &Var, expr : Expr) -> Vec<Equation> {
    let mut equations = past_equ.clone();
    equations.push(build_equation(v.clone(), expr));
    equations
}

// check if an expression is an Efby
pub fn is_fby(expr : &Expr) -> bool {
    match expr{
        Expr::Efby(_,_) => true,
        _ => false,
    }
}

// returns type of a constant
pub fn type_cst(c : &Constant) -> Typ {
    match c{
        Constant::Cint(_) => Typ::Tint,
        Constant::Cbool(_) => Typ::Tbool,
        Constant::Cfloat(_)=> Typ::Treal,
    }
}

// returns type of an expression
pub fn type_expr(expr : &Expr) -> Typ {
    match expr{
        Expr::Econst(c) => type_cst(c),
        Expr::Evar(v) => v.vtype.clone(),
        Expr::Ebinop(_, e, _ )
        | Expr::Eunop(_, e)
        | Expr::Efby(e, _)
        | Expr::Emerge(_, e, _)
        | Expr::Ewhen(e, _)
        => type_expr(e),
        Expr::Ecall(_,_) => panic!(),
        _ => panic!(),
    }
}



/* Utility functions from AST lustre to AST in C */

// Checks that a variable in lustre (Var) is eq to a variable in C (CVar)
pub fn eq_var_lc(vl: &Var, vc: &CVar) -> bool {
    vl.id == vc.id
}

// Given a state and the variable,
// finds a CVar equal to the variable
// and modifies its depth
pub fn modify_height(mut state: CState, v: &Var, depth: i32) -> CState {
    for i in 0..state.vars.len() {
        if eq_var_lc(v, &state.vars[i]) {
            state.vars[i].depth = depth
        }
    }
    state
}

// builds a base state associated to a node
// in this base state, all variables have depth 0
pub fn build_base_cstate(node: &Node) -> CState {
    let mut state = astc::empty_state();
    for var in &node.input {
        state.vars.push(astc::build_cvar(
            var.name.clone(),
            var.id,
            var.vtype.clone(),
            0,
            CVarRole::Input,
            None,
        ));
    }
    for var in &node.output {
        state.vars.push(astc::build_cvar(
            var.name.clone(),
            var.id,
            var.vtype.clone(),
            0,
            CVarRole::Output,
            None,
        ));
    }
    for var in &node.local_vars {
        state.vars.push(astc::build_cvar(
            var.name.clone(),
            var.id,
            var.vtype.clone(),
            0,
            CVarRole::LocalVar,
            None,
        ));
    }
    state
}

//checks if a variable is in state
//if it is not, then it is in memory
pub fn in_cstate(var : &Var, cprogram : &CProg) -> bool{
    for vc in &cprogram.state.vars {
        if eq_var_lc(var, vc) {
            return true
        }
    }
    false
}