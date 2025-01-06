use std::collections::HashMap;

use astlustre::{Equation, Var};
use utilityastlustre::{gather_vars, out_of_pre};

pub mod astc;
pub mod astlustre;
pub mod compile;
pub mod utilityastlustre;

// utility type for scheduling
// def : defined variable
// vars : variables appearing in expression
// left : variables appearing free in expression and not as an argument of a delay fby
pub struct EquationInfo {
    pub equ: Equation,
    pub def: Var,
    pub vars: Vec<Var>,
    pub left: Vec<Var>,
}

pub type DInfo = HashMap<usize, EquationInfo>;

// build an equation_info from an equation
pub fn build_equ_info(equ: &Equation) -> EquationInfo {
    EquationInfo {
        equ: equ.clone(),
        def: equ.var.clone(),
        vars: gather_vars(&equ.expression),
        left: out_of_pre(&equ.expression),
    }
}

// build a d_info for a set of equations
pub fn build_d_info(body: &Vec<Equation>) -> DInfo {
    let mut d = HashMap::new();
    for i in 0..body.len() {
        d.insert(i, build_equ_info(&body[i]));
    }
    d
}
