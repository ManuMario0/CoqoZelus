use crate::compiler::{
    astc::{self, build_cprog, build_local_cvar, build_mem, empty_mem, new_localcvar},
    astlustre::{
        self, Equation, Expr, LustreProg, Node
    },
    utilityastlustre::{
        self, build_base_cstate, build_equation, build_node, modify_height, new_localvar, number_vars, type_cst, var_in
    }
};

use crate::transpile::{CLocalVar, CMemory, CProg, CState, CStep, CVar};

use super::utilityastlustre::{is_fby, new_fbyvar, push_equ, type_expr};

// TODO
// anti dependency
// check normalization
// finish execution


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

/* NORMALISATION PHASE
source-to-source transformation which consists in extracting stateful
computations that appear inside expressions */

// transforms a node's body into a normalized body
// and returns the node, as well as a CMemory of the created local variables
pub fn normalize_body(node : &Node) -> Node {
    let body = normalize_equations(&node.body);
    build_node(node.name.clone(),
    node.input.clone(),
    node.output.clone(),
    node.local_vars.clone(),
    body)
}

pub fn normalize_equations(body : &Vec<Equation>) -> Vec<Equation> {
    let mut normed_equations = Vec::new();
    let mut nb_fby = 0;
    let mut nb_equ = 0;
    for equ in body {
        let mut equations = normalize_equ(&equ, nb_equ);
        equations.reverse();
        normed_equations.extend(equations);
        nb_equ = nb_equ+1;
    }
    normed_equations
}

// in paper : NormD
// these all mirror the wanted list !!!!!!!!!!!!
pub fn normalize_equ(equ : &Equation, nb_equ : i32) -> Vec<Equation> {
    match &equ.expression{
        Expr::Efby(v, e) => {
            let vtype = equ.var.vtype.clone();
            let fbyvar = new_fbyvar(nb_equ.try_into().unwrap(), vtype);
            let (e, equs, _) = normalize_expression_aux(&e, &Vec::new(), nb_equ, 0);

            let fbyexpr = Expr::Efby(v.clone(), Box::new(Expr::Evar(fbyvar.clone())));
            // first, we push fbyvar = e
            let equs = push_equ(equs, &fbyvar, e);
            // then, we push equ.var = v fby fbyvar
            push_equ(equs, &equ.var, fbyexpr)
            // I JUST REALIZED THIS PUSHES IN THE WRONG ORDER
            // TODO FIX !!
        },
        Expr::Ecall(_, _) => panic!(),
        _ => {
            let (e, equs, _) = normalize_expression_merge(&equ.expression, &Vec::new(), nb_equ, 0);
            // push equ.var = e
            push_equ(equs, &equ.var, e)
        },
    }
}

// in paper : NormCA
pub fn normalize_expression_merge(expr : &Expr, equs : &Vec<Equation>, nb_equ : i32, n : usize) -> (Expr, Vec<Equation>, usize) {
    match expr{
        Expr::Emerge(v,e1 ,e2 ) => {
            let (normed_e1, equs, n) = normalize_expression_merge(e1, equs, nb_equ, n);
            let (normed_e2, equs, n) = normalize_expression_merge(e2, &equs, nb_equ, n);
            (Expr::Emerge(v.clone(), Box::new(normed_e1), Box::new(normed_e2)),
            equs, n)
        },
        Expr::Ecall(_,_ ) => panic!(),
        _ => normalize_expression_aux(expr, equs, nb_equ, n),
    }
}

// in paper : NormE
pub fn normalize_expression_aux(expr : &Expr, equs : &Vec<Equation>, nb_equ : i32, n : usize) -> (Expr, Vec<Equation>, usize) {
    match expr{
        Expr::Econst(_) => (expr.clone(), equs.clone(), n),
        Expr::Evar(_) => (expr.clone(), equs.clone(), n),
        Expr::Ebinop(b, e1, e2)=> {
            let (normed_e1, equs, n) = normalize_expression_aux(e1, equs, nb_equ, n);
            let (normed_e2, equs, n) = normalize_expression_aux(e2, &equs, nb_equ, n);
            (Expr::Ebinop(b.clone(), Box::new(normed_e1), Box::new(normed_e2)),
            equs, n)
        } ,
        Expr::Eunop(u, e)=> {
            let (normed_e, equs, n) = normalize_expression_aux(e, equs, nb_equ, n);
            (Expr::Eunop(u.clone(), Box::new(normed_e)),
            equs, n)
        },
        Expr::Ewhen(e, v) => {
            let (normed_e, equs, n) = normalize_expression_aux(e, equs, nb_equ, n);
            (Expr::Ewhen(Box::new(normed_e), v.clone()),
            equs, n)
        }
        Expr::Efby(v, e) => {
            let (normed_e, equs, n) = normalize_expression_aux(e, equs, nb_equ, n);
            let new_var = new_localvar(n, nb_equ, type_expr(v));
            // push new_var = v fby normed_e
            let equs = push_equ(equs, &new_var, Expr::Efby(v.clone(), Box::new(normed_e)));
            (Expr::Evar(new_var), equs, n+1)
        } ,
        Expr::Emerge(v, e1, e2) => {
            let (normed_e1, equs, n) = normalize_expression_merge(e1, equs, nb_equ, n);
            let (normed_e2, equs, n) = normalize_expression_merge(e2, &equs, nb_equ, n);
            let new_var = new_localvar(n, nb_equ, type_expr(e1));
            // push new_var = merge v normed_e1 normed_e2
            let equs = push_equ(equs, &new_var, Expr::Emerge(v.clone(), Box::new(normed_e1), Box::new(normed_e2)));
            (Expr::Evar(new_var), equs, n+1)
        },
        _ => normalize_expression_merge(expr, equs, nb_equ, n)
    }
}


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
