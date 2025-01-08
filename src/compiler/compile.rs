use std::ops::Not;

use crate::{
    compiler::{
        astc::{self, build_cprog, empty_step},
        astlustre::{self, Equation, Expr, LustreProg, Node},
        utilityastlustre::{
            self, build_base_cprog, build_equation, build_node,
            new_localvar, number_vars, type_cst, var_in,
        },
    },
    transpile::{CVarRole, Cexpr},
};

use crate::transpile::{CProg, CState, CStep, CVar, Cinstruction};

use super::{
    build_d_info,
    utilityastlustre::{
        contains_var, gather_vars, new_fbyvar, out_of_pre, push_equ, trans_boolvar,
        trans_var, translate_const, translate_var, type_expr, var_in_prog,
    },
    DInfo,
};

/* ------------------------------------------------------------------------ */

/* NORMALISATION PHASE

source-to-source transformation which consists in extracting stateful
computations that appear inside expressions */

// transforms a node's body into a normalized body
// and returns the node, as well as a CMemory of the created local variables
pub fn normalize_body(node: &Node) -> Node {
    let body = normalize_equations(&node.body);
    build_node(
        node.name.clone(),
        node.input.clone(),
        node.output.clone(),
        node.local_vars.clone(),
        body,
    )
}

// in paper : NormD, case D_1 and D_2
pub fn normalize_equations(body: &Vec<Equation>) -> Vec<Equation> {
    let mut normed_equations = Vec::new();
    let mut nb_equ = 0;
    for equ in body {
        let mut equations = normalize_equ(&equ, nb_equ);
        equations.reverse(); // this should fix things being pushed in the wrong order
        normed_equations.extend(equations);
        nb_equ = nb_equ + 1;
    }
    normed_equations
}

// in paper : NormD
pub fn normalize_equ(equ: &Equation, nb_equ: i32) -> Vec<Equation> {
    match &equ.expression {
        Expr::Efby(v, e) => {
            let vtype = equ.var.vtype.clone();

            let fbyvar = new_fbyvar(nb_equ.try_into().unwrap(), &vtype, 0);
            let fbyvarinter = new_fbyvar(nb_equ.try_into().unwrap(), &vtype, 1);
            let (e, equs, _) = normalize_expression_aux(&e, &Vec::new(), nb_equ, 0);

            let fbyexpr = Expr::Efby(v.clone(), Box::new(Expr::Evar(fbyvar.clone())));
            // first, we push fbyvar = e
            let equs = push_equ(equs, &fbyvar, e);
            // then, we push fbyvarinter = v fby fbyvar
            let equs = push_equ(equs, &fbyvarinter, fbyexpr);
            // then, we push equ.var = fbyvarinter
            push_equ(equs, &equ.var, Expr::Evar(fbyvarinter))
        }
        Expr::Ecall(_, _) => panic!(),
        _ => {
            let (e, equs, _) = normalize_expression_merge(&equ.expression, &Vec::new(), nb_equ, 0);
            // push equ.var = e
            push_equ(equs, &equ.var, e)
        }
    }
}

// in paper : NormCA
pub fn normalize_expression_merge(
    expr: &Expr,
    equs: &Vec<Equation>,
    nb_equ: i32,
    n: usize,
) -> (Expr, Vec<Equation>, usize) {
    match expr {
        Expr::Emerge(v, e1, e2) => {
            let (normed_e1, equs, n) = normalize_expression_merge(e1, equs, nb_equ, n);
            let (normed_e2, equs, n) = normalize_expression_merge(e2, &equs, nb_equ, n);
            (
                Expr::Emerge(v.clone(), Box::new(normed_e1), Box::new(normed_e2)),
                equs,
                n,
            )
        }
        Expr::Ecall(_, _) => panic!(),
        _ => normalize_expression_aux(expr, equs, nb_equ, n),
    }
}

// in paper : NormE
pub fn normalize_expression_aux(
    expr: &Expr,
    equs: &Vec<Equation>,
    nb_equ: i32,
    n: usize,
) -> (Expr, Vec<Equation>, usize) {
    match expr {
        Expr::Econst(_) => (expr.clone(), equs.clone(), n),
        Expr::Evar(_) => (expr.clone(), equs.clone(), n),
        Expr::Ebinop(b, e1, e2) => {
            let (normed_e1, equs, n) = normalize_expression_aux(e1, equs, nb_equ, n);
            let (normed_e2, equs, n) = normalize_expression_aux(e2, &equs, nb_equ, n);
            (
                Expr::Ebinop(b.clone(), Box::new(normed_e1), Box::new(normed_e2)),
                equs,
                n,
            )
        }
        Expr::Eunop(u, e) => {
            let (normed_e, equs, n) = normalize_expression_aux(e, equs, nb_equ, n);
            (Expr::Eunop(u.clone(), Box::new(normed_e)), equs, n)
        }
        Expr::Ewhen(e, v) => {
            let (normed_e, equs, n) = normalize_expression_aux(e, equs, nb_equ, n);
            (Expr::Ewhen(Box::new(normed_e), v.clone()), equs, n)
        }
        Expr::Efby(c, e) => {
            let (normed_e, equs, n) = normalize_expression_aux(e, equs, nb_equ, n);
            let new_var = new_localvar(n, nb_equ, type_cst(c));
            // push new_var = v fby normed_e
            let equs = push_equ(equs, &new_var, Expr::Efby(c.clone(), Box::new(normed_e)));
            (Expr::Evar(new_var), equs, n + 1)
        }
        Expr::Emerge(v, e1, e2) => {
            let (normed_e1, equs, n) = normalize_expression_merge(e1, equs, nb_equ, n);
            let (normed_e2, equs, n) = normalize_expression_merge(e2, &equs, nb_equ, n);
            let new_var = new_localvar(n, nb_equ, type_expr(e1));
            // push new_var = merge v normed_e1 normed_e2
            let equs = push_equ(
                equs,
                &new_var,
                Expr::Emerge(v.clone(), Box::new(normed_e1), Box::new(normed_e2)),
            );
            (Expr::Evar(new_var), equs, n + 1)
        }
        _ => normalize_expression_merge(expr, equs, nb_equ, n),
    }
}

/* ------------------------------------------------------------------------ */

/* SCHEDULING PHASE

find a schedule of a vector of equations
results in the reordering of the Node's body

a schedule needs to respect these constraints :
- static dependency
- anti dependency on fby */

// check if an equation is ready (i.e. it can be inserted in the schedule)
// takes the equation and the set of other equations waiting to be scheduled
pub fn is_ready(equ: &Equation, key: &usize, d: &DInfo) -> bool {
    match &equ.expression {
        Expr::Efby(_, e) => {
            let vars_in_e = gather_vars(e);
            for (i, equ_info) in d {
                if key == i {
                    continue;
                }
                if var_in(&equ.var, &equ_info.vars) {
                    return false;
                }
                if var_in(&equ_info.def, &vars_in_e) {
                    return false;
                }
            }
            true
        }
        _ => {
            for (i, equ_info) in d {
                if key == i {
                    continue;
                }
                if var_in(&equ.var, &equ_info.left) {
                    return false;
                }
                if var_in(&equ.var, &out_of_pre(&equ.expression)) {
                    return false;
                }
            }
            true
        }
    }
}

// finds a correct scheduling of a vector of equations
// fails if it does not find one
pub fn schedule(body: Vec<Equation>) -> Vec<Equation> {
    let mut schedule = Vec::new();
    let mut d = build_d_info(&body);
    while !d.is_empty() {
        let mut n: Option<usize> = None;
        for (key, einfo) in &d {
            if is_ready(&einfo.equ, key, &d) {
                n = Some(*key);
                break;
            }
        }
        match n {
            Some(n) => {
                let einfo = d.remove(&n);
                schedule.push(einfo.unwrap().equ);
            }
            None => panic!("Dependency graph is not acyclic"),
        }
    }
    schedule
}

/* ------------------------------------------------------------------------ */

/* TRANSLATION PHASE
transform normalized node into an astc
devided into two part : translate_vars and translate_step


NOTE : every trans_var call is an absurd amount of time wasted,
    but the fix seems complicated*/

/* -------------------- */

// takes the node
// and returns an incomplete CProg (with empty step field)
pub fn translate_vars(node: &Node) -> CProg {
    let mut prog = build_base_cprog(node);
    for equ in &node.body {
        match &equ.expression {
            Expr::Efby(v, _) => {
                if !contains_var(&equ.var, &prog.state.vars) {
                    prog.state
                        .vars
                        .push(translate_var(&equ.var, CVarRole::LocalVar, Some(v)))
                }
            }
            _ => {
                if !var_in_prog(&equ.var, &prog) {
                    prog.local_vars
                        .push(translate_var(&equ.var, CVarRole::LocalVar, None));
                }
            }
        }
    }
    prog
}

/* -------------------- */

// takes the node, and an incomplete CProg (with empty step field)
// and returns a CProg with the steps
pub fn translate_steps(node: &Node, mut prog: CProg) -> CProg {
    prog.step = translate_eqlist(&node.body, &prog);
    prog
}

// in paper : TEqList
pub fn translate_eqlist(equs: &Vec<Equation>, prog: &CProg) -> CStep {
    let mut steps = empty_step();
    for equ in equs {
        let instruction = translate_eq(equ, prog);
        steps.body.push(instruction)
    }
    steps
}

// in paper : TEq
pub fn translate_eq(equ: &Equation, prog: &CProg) -> Cinstruction {
    match &equ.expression {
        Expr::Efby(_, e) => {
            // equ.var is in state
            let trans_e = translate_expression(e, &prog);
            Cinstruction::Cassign(trans_var(&equ.var, prog), trans_e)
        }
        Expr::Ecall(_, _) => panic!(),
        _ => translate_assign(equ, &prog),
    }
}

// in paper : TA
pub fn translate_assign(equ: &Equation, prog: &CProg) -> Cinstruction {
    match &equ.expression {
        Expr::Emerge(v, e1, e2) => {
            let trans_equ1 = translate_assign(&build_equation(&equ.var, &e1), prog);
            let trans_equ2 = translate_assign(&build_equation(&equ.var, &e2), prog);
            Cinstruction::Ccase(
                trans_var(v, prog),
                Box::new(trans_equ1),
                Box::new(trans_equ2),
            )
        }
        _ => {
            let trans_expr = translate_expression(&equ.expression, &prog);
            Cinstruction::Cassign(trans_var(&equ.var, prog), trans_expr)
        }
    }
}

// in paper : TE
pub fn translate_expression(expr: &Expr, prog: &CProg) -> Cexpr {
    match expr {
        Expr::Econst(c) => Cexpr::Cconst(translate_const(c)),
        Expr::Evar(v) => Cexpr::Cvar(trans_var(v, prog)),
        Expr::Ebinop(b, e1, e2) => {
            let trans_e1 = translate_expression(e1, prog);
            let trans_e2 = translate_expression(e2, prog);
            Cexpr::Cbinop(b.clone(), Box::new(trans_e1), Box::new(trans_e2))
        }
        Expr::Eunop(u, e1) => {
            let trans_e1 = translate_expression(e1, prog);
            Cexpr::Cunop(u.clone(), Box::new(trans_e1))
        }
        Expr::Ewhen(e, v) => {
            let trans_e = translate_expression(e, prog);
            Cexpr::Cwhen(Box::new(trans_e), trans_boolvar(v, prog))
        }
        _ => todo!(),
    }
}

/* ------------------------------------------------------------------------ */

// Takes a Lustre AST and translates it into a C AST
pub fn compile(ast: LustreProg) -> CProg {
    let mut node = ast.get(0).unwrap().clone();
    node = normalize_body(&node);
    let schedule = schedule(node.body);
    node.body = schedule;
    let mut prog = translate_vars(&node);
    translate_steps(&node, prog)
}
