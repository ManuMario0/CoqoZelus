/* Utility functions for C syntactic tree */

use crate::transpile::{CConst, CProg, CState, CStep, CVar, CVarRole, Cinstruction};

use super::astlustre::Typ;

// creates an empty CProg
pub fn empty_cprog() -> CProg {
    CProg {
        state: empty_state(),
        inputs: Vec::new(),
        outputs: Vec::new(),
        local_vars: Vec::new(),
        step: empty_step(),
    }
}

// build a CProg
pub fn build_cprog(
    state: CState,
    inputs: Vec<CVar>,
    outputs: Vec<CVar>,
    local_vars: Vec<CVar>,
    step: CStep,
) -> CProg {
    CProg {
        state,
        inputs,
        outputs,
        local_vars,
        step,
    }
}

// creates an empty state
pub fn empty_state() -> CState {
    CState { vars: Vec::new() }
}

// creates an empty step
pub fn empty_step() -> CStep {
    CStep { body: Vec::new() }
}

// builds a step
pub fn build_step(body: Vec<Cinstruction>) -> CStep {
    CStep { body }
}

// build a cvar
pub fn build_cvar(
    name: String,
    id: usize,
    typ: Typ,
    depth: i32,
    role: CVarRole,
    init_value: Option<CConst>,
) -> CVar {
    CVar {
        name,
        id,
        vtype: typ,
        depth,
        role,
        init_value,
    }
}

// checks that two cvariables are equal
pub fn eq_var_c(x: &CVar, y: &CVar) -> bool {
    x.name == y.name
}
