
/* Utility functions for AST LUSTRE */

use core::panic;
use std::ops::Not;

use crate::{compiler::{astc::{self}, astlustre::{Equation, Expr, Node, Var}}, transpile::{CConst, CState}};

use crate::transpile::{CProg, CVar, CVarRole};

use super::astlustre::{Constant, Typ};

pub fn build_node(name : String, input : Vec<Var>, output : Vec<Var>, local_vars : Vec<Var>, body : Vec<Equation>) -> Node {
    Node { name, input, output, local_vars, body}
}

pub fn build_equation(var : &Var, expression : &Expr) -> Equation {
    Equation { var: var.clone(), expression : expression.clone() }
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

// Checks if two var vectors have a var in common
pub fn var_in_common(v1 : &Vec<Var>, v2 : &Vec<Var>) -> bool {
    for v in v1 {
        if var_in(v, v2) {
            return true
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
    equations.push(build_equation(&v, &expr));
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
        Expr::Econst(c)
        | Expr::Efby(c, _) => type_cst(c),
        Expr::Evar(v) => v.vtype.clone(),
        Expr::Ebinop(_, e, _ )
        | Expr::Eunop(_, e)
        | Expr::Emerge(_, e, _)
        | Expr::Ewhen(e, _)
        => type_expr(e),
        Expr::Ecall(_,_) => panic!(),
        _ => panic!(),
    }
}

// gathers variables of an expression
pub fn gather_vars(expr: &Expr) -> Vec<Var> {
    gather_vars_aux(expr, Vec::new())
}

pub fn gather_vars_aux(expr : &Expr, mut vars : Vec<Var>) -> Vec<Var> {
    match expr{
        Expr::Econst(_) => vars,
        Expr::Evar(v) => {
            vars.push(v.clone());
            vars
        },
        Expr::Ebinop(_,e1 ,e2 )
        | Expr::Emerge(_,e1 ,e2 )
        => {
            let vars = gather_vars_aux(e1, vars);
            gather_vars_aux(e2, vars)
        },
        Expr::Eunop(_, e)
        | Expr::Efby(_, e)
        | Expr::Ewhen(e, _)
        => gather_vars_aux(e, vars),
        Expr::Ecall(_, vs) => {
            vars.extend(vs.clone());
            vars
        }
        _ => panic!(),
    }
}


/* SYNTACTIC DEPENDENCY :
the minimal requirement for a suitable computation order
X depends on Y iff X=E and Y appears outside of a pre operator in E
A program is causal when for each node the corresponding graph of dependencies is acyclic */

/* 
// builds the syntactic dependency of variables in a program
// in the form of an adjacency list
pub fn dependency_list(node: &Node) -> Vec<Vec<astlustre::Var>> {
    let mut deplist = vec![Vec::new(); number_vars(node)];
    for eq in &node.body {
        deplist[eq.var.id] = out_of_pre(&eq.var, &eq.expression)
    }
    deplist
}*/


// find the variables that appears outside of a pre operator in expression E
pub fn out_of_pre(expr: &Expr) -> Vec<Var> {
    outside_of_pre(expr, Vec::new())
}

pub fn outside_of_pre(
    expr: &Expr,
    mut vars: Vec<Var>,
) -> Vec<Var> {
    match expr {
        Expr::Econst(_) => vars,
        Expr::Evar(v) => {
            if var_in(&v, &vars).not() {
                vars.push(v.clone())
            }
            vars
        }
        Expr::Ebinop(_, e1, e2)
        | Expr::Emerge(_,e1 ,e2 ) => {
            let vars = outside_of_pre(&*e1, vars);
            outside_of_pre(&*e2, vars)
        }
        Expr::Eunop(_, e) => outside_of_pre(&*e, vars),
        Expr::Eifthenelse(e1, e2, e3) => {
            let vars = outside_of_pre(&*e1, vars);
            let vars = outside_of_pre(&*e2, vars);
            outside_of_pre(&*e3, vars)
        }
        Expr::Earrow(e1, e2) => {
            let vars = outside_of_pre(&*e1, vars);
            outside_of_pre(&*e2, vars)
        }
        Expr::Epre(e1) => vars,
        Expr::Efby(c, e) => vars,
        Expr::Ewhen(e, _) => outside_of_pre(e, vars),
        Expr::Ecall(_, _) => vars, //TODO
    }
}




/* Utility functions from AST lustre to AST in C */

// Checks that a variable in lustre (Var) is eq to a variable in C (CVar)
pub fn eq_var_lc(vl: &Var, vc: &CVar) -> bool {
    vl.id == vc.id && vl.name == vc.name
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

// given a vector of cvars and a var, finds the cvar equal to the var
pub fn trans_var_aux(vl : &Var, lvc : &Vec<CVar>) -> Option<CVar> {
    for vc in lvc {
        if eq_var_lc(vl, vc) {
            return Some(vc.clone())
        }
    }
    None
}

// given a prog and a var, checks if var is in input, output or local_var
pub fn trans_var(v : &Var, prog : &CProg) -> CVar {
    match trans_var_aux(v, &prog.inputs){
        Some(vc) => vc,
        None => match trans_var_aux(v, &prog.outputs){
            Some(vc) => vc,
            None => match trans_var_aux(v, &prog.local_vars){
                Some(vc) => vc,
                None => panic!("should not happen : could not find cvar in prog")
            }
        }
    }
}

// given a vector of cvars and a var, checks if there is a cvar equal to the var
pub fn contains_var(vl : &Var, lvc : &Vec<CVar>) -> bool {
    for vc in lvc {
        if eq_var_lc(vl, vc) {
            return true
        }
    }
    false
}

// given a prog and a var, checks if var is in input, output or local_var
pub fn var_in_prog(v : &Var, prog : &CProg) -> bool {
    contains_var(v, &prog.inputs)
    || contains_var(v, &prog.outputs)
    || contains_var(v, &prog.local_vars)
}

// translate a Constant into a CConst (this is straightforward)
pub fn translate_const(c : &Constant) -> CConst {
    match c{
        Constant::Cbool(b) => CConst::CCbool(*b),
        Constant::Cfloat(f) => CConst::CCfloat(*f),
        Constant::Cint(i) => CConst::CCint(*i),
    }
}


// translate a Constant into a CConst with options (this is straightforward)
pub fn translate_const_aux(c : Option<&Constant>) -> Option<CConst> {
    match c{
        None => None,
        Some(Constant::Cbool(b)) => Some(CConst::CCbool(*b)),
        Some(Constant::Cfloat(f)) => Some(CConst::CCfloat(*f)),
        Some(Constant::Cint(i)) => Some(CConst::CCint(*i)),
    }
}

//translates a var into a cvar (when its role is known) (option : initial value)
pub fn translate_var(var : &Var, role : CVarRole, init_value : Option<&Constant>) -> CVar {
    astc::build_cvar(var.name.clone(), var.id, var.vtype.clone(), 0, role, translate_const_aux(init_value))
}

// builds a base state associated to a node
// in this base state, all variables have depth 0
pub fn build_base_cprog(node: &Node) -> CProg {
    let mut prog = astc::empty_cprog();
    for var in &node.input {
        prog.inputs.push(translate_var(var, CVarRole::Input, None))
    }
    for var in &node.output {
        prog.outputs.push(translate_var(var, CVarRole::Output, None))
    }
    for var in &node.local_vars {
        prog.local_vars.push(translate_var(var, CVarRole::LocalVar, None))
    }
    prog
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