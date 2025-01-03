use crate::compiler::{
    astc::{self, build_cprog, build_local_cvar, build_mem, empty_mem, new_localcvar},
    astlustre::{
        self, Equation, Expr, LustreProg, Node
    },
    utilityastlustre::{
        self, build_base_cstate, build_equation, build_node, modify_height, number_vars, var_in
    }
};

use crate::transpile::{CLocalVar, CMemory, CProg, CState, CStep, CVar};

/* SYNTACTIC DEPENDENCY :
the minimal requirement for a suitable computation order
X depends on Y iff X=E and Y appears outside of a pre operator in E
A program is causal when for each node the corresponding graph of dependencies is acyclic */

// builds the syntactic dependency of variables in a program
// in the form of an adjacency list
pub fn dependency_list(node: &Node) -> Vec<Vec<astlustre::Var>> {
    let mut deplist = vec![Vec::new(); number_vars(node)];
    for eq in &node.body {
        deplist[eq.var.id] = out_of_pre(&eq.var, &eq.expression)
    }
    deplist
}

// checks acyclicity of the dependency graph
pub fn acyclicity(_dependency_graph: Vec<Vec<astlustre::Var>>) -> bool {
    todo!()
}

// find the variables that appears outside of a pre operator in expression E
pub fn out_of_pre(y: &astlustre::Var, expr: &Expr) -> Vec<astlustre::Var> {
    outside_of_pre(y, expr, Vec::new())
}

pub fn outside_of_pre(
    y: &astlustre::Var,
    expr: &Expr,
    mut vars: Vec<astlustre::Var>,
) -> Vec<astlustre::Var> {
    match expr {
        Expr::Econst(_) => vars,
        Expr::Evar(v) => {
            if var_in(&v, &vars) {
                vars.push(v.clone())
            }
            vars
        }
        Expr::Ebinop(_, e1, e2)
        | Expr::Emerge(_,e1 ,e2 ) => {
            let vars = outside_of_pre(&y, &*e1, vars);
            outside_of_pre(&y, &*e2, vars)
        }
        Expr::Eunop(_, e) => outside_of_pre(&y, &*e, vars),
        Expr::Eifthenelse(e1, e2, e3) => {
            let vars = outside_of_pre(&y, &*e1, vars);
            let vars = outside_of_pre(&y, &*e2, vars);
            outside_of_pre(&y, &*e3, vars)
        }
        Expr::Earrow(e1, e2) => {
            let vars = outside_of_pre(&y, &*e1, vars);
            outside_of_pre(&y, &*e2, vars)
        }
        Expr::Epre(e1) => vars,
        Expr::Efby(e1, e2) => {
            outside_of_pre(&y, &*e1, vars)
        }
        Expr::Ewhen(e, _) => outside_of_pre(y, e, vars),
        Expr::Ecall(_, _) => vars, //TODO
    }
}

/* ------------------------------------------------------------------------- */

// builds the CState
/*  the notable part is finding the depth of variables in the node */
pub fn gather_cstate(node: &Node) -> CState {
    let state = build_base_cstate(node);
    assign_depths(state, &node.body)
}

// assign heights to all variables
pub fn assign_depths(mut state: CState, eqs: &Vec<Equation>) -> CState {
    for eq in eqs {
        state = assign_depths_aux(&state, &eq.expression, 0);
    }
    state
}

// auxiliary recursive function
pub fn assign_depths_aux(state: &CState, expr: &Expr, current_depth: i32) -> CState {
    match expr {
        Expr::Econst(_) => state.clone(),
        Expr::Evar(v) => modify_height(state.clone(), &v, current_depth),
        Expr::Ebinop(_, e1, e2)
        | Expr::Earrow(e1, e2)
        | Expr::Emerge(_, e1 , e2 ) => {
            let state = assign_depths_aux(state, &*e1, current_depth);
            assign_depths_aux(&state, &*e2, current_depth)
        }
        Expr::Eunop(_, e) | Expr::Ewhen(e, _) => assign_depths_aux(state, e, current_depth),
        Expr::Eifthenelse(e1, e2, e3) => {
            let state = assign_depths_aux(state, &*e1, current_depth);
            let state = assign_depths_aux(&state, &*e2, current_depth);
            assign_depths_aux(&state, &*e3, current_depth)
        }
        Expr::Epre(e) => assign_depths_aux(state, &*e, current_depth + 1),
        Expr::Efby(e1, e2) => {
            let state = assign_depths_aux(state, &*e1, current_depth);
            assign_depths_aux(&state, &*e2, current_depth + 1)
        }
        Expr::Ecall(_, _) => todo!(),
    }
}

/* ------------------------------------------------------------------------ */

/*
/* NORMALISATION PHASE
source-to-source transformation which consists in extracting stateful
computations that appear inside expressions */

// transforms a node's body into a normalized body
// and returns the node, as well as a CMemory of the created local variables
pub fn normalize_body(node : &Node) -> (Node, CMemory) {
    let (body, memory) = normalize_equations(&node.body);
    (build_node(node.name.clone(),
    node.input.clone(),
    node.output.clone(),
    node.local_vars.clone(),
    body),
    build_mem(memory))
}

pub fn normalize_equations(body : &Vec<Equation>) -> (Vec<Equation>, Vec<CLocalVar>) {
    let mut normed_equations = Vec::new();
    let mut vars = Vec::new();
    for equ in body {
        let (equations, local_vars) = normalize_equ(&equ, Vec::new());
        normed_equations.extend(equations);
        vars.extend(local_vars);
        // CHECK IF EXTEND IS AN OKAY FUNCTION
    }
    (normed_equations, vars)
}

pub fn normalize_equ(equ : &Equation, local_vars : Vec<CLocalVar>) -> (Vec<Equation>, Vec<CLocalVar>) {
    match equ.expression{
        //Expr::Eifthenelse(Box<Expr>, Box<Expr>, Box<Expr>),
        //Expr::Earrow(Box<Expr>, Box<Expr>),
        //Expr::Epre(Box<Expr>),
        Expr::Efby(v, e) => {
            let (normed_expr, local_vars) = normalize_expression_aux(&e, local_vars);
            (build_equation(equ.var, Expr::Efby(v.clone(), Box::new(normed_expr))),
            local_vars)
        }
        Expr::Emerge(_, _, _) => {
            normalize_expression_merge(expr, local_vars)
        } ,
        //Expr::Ecall(s, vars) =>,
        _ => normalize_expression_aux(expr, local_vars),
    }
}

pub fn normalize_expression_merge(expr : &Expr, local_vars : Vec<CLocalVar>) -> (Expr, Vec<Equation>, Vec<CLocalVar>) {
    match expr{
        Expr::Emerge(v,e1 ,e2 ) => {
            let (normed_e1, local_vars) = normalize_expression_merge(e1, local_vars);
            let (normed_e2, local_vars) = normalize_expression_merge(e2, local_vars);
            (Expr::Emerge(v.clone(), Box::new(normed_e1), Box::new(normed_e2)), local_vars)
        },
        _ => normalize_expression_aux(expr, local_vars),
    }
}

pub fn normalize_expression_aux(expr : &Expr, local_vars : Vec<CLocalVar>) -> (Expr, Vec<Equation>, Vec<CLocalVar>) {
    normalize_expression_nb(expr, local_vars, 0)
}

pub fn normalize_expression_nb(expr : &Expr, equations : Vec<Equation>, local_vars : Vec<CLocalVar>, n : usize) -> (Expr, Vec<Equation>, Vec<CLocalVar>, usize) {
    match expr{
        Expr::Econst(_) => (expr.clone(), equations, local_vars),
        Expr::Evar(_) => (expr.clone(), equations, local_vars),
        Expr::Ebinop(b, e1, e2)=> {
            let (normed_e1, equations, local_vars, n) = normalize_expression_nb(e1, equations, local_vars, n);
            let (normed_e2, equations, local_vars, n) = normalize_expression_nb(e2, equations, local_vars, n);
            (Expr::Ebinop(b.clone(), Box::new(normed_e1), Box::new(normed_e2)),
            equations, local_vars, n)
        } ,
        Expr::Eunop(u, e)=> {
            let (normed_e, equations, local_vars, n) = normalize_expression_nb(e, equations, local_vars, n);
            (Expr::Eunop(u.clone(), Box::new(normed_e)),
            equations, local_vars, n)
        },
        //Expr::Eifthenelse(Box<Expr>, Box<Expr>, Box<Expr>),
        //Expr::Earrow(Box<Expr>, Box<Expr>),
        //Expr::Epre(Box<Expr>),
        Expr::Efby(v, e) => {
            let (normed_e, equations, local_vars, n) = normalize_expression_nb(e, equations, local_vars, n);
            let new_var = new_localcvar(n);
            local_vars.push(new_var);
            (Expr::Evar(new_var) )
        } ,
        Expr::Ewhen(e, v) => todo!(),id
        Expr::Emerge(v, e1, e2) => todo!(),
        Expr::Ecall(s, vars) => todo!(),
        _ => todo!()
    }
    // global pattern matching
}
*/

/* ------------------------------------------------------------------------ */

/* TRANSLATION PHASE
transform normalized node into an astc */

pub fn translate(node : &Node,
    cstate : CState,
    cmem : CMemory,
    inputs : Vec<CVar>,
    outputs : Vec<CVar>)
    -> CProg {
    todo!()
    //build_cprog(cstate, cmem, inputs, outputs, translate_body(&node.body))
}

pub fn translate_eq(cstate : CState, cmem : CMemory, equation : &Equation) -> CProg {
    todo!()
}

pub fn translate_expression(cstate : CState, cmem : CMemory, expr: &Expr) -> CProg {
    todo!()
}

/* ------------------------------------------------------------------------ */

// Takes a Lustre AST and translates it into a C AST
pub fn compile(ast: LustreProg) -> CProg {
    //let state = astc::empty_state();
    let node = ast.get(0).unwrap();
    let state = gather_cstate(node);
    build_cprog(state, empty_mem(), Vec::new(), Vec::new(), CStep { body: Vec::new() })
}
