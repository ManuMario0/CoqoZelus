/* Utility functions for C syntactic tree */

use crate::transpile::{CConst, CProg, CState, CStep, CVar, CVarRole, Cexpr, Cinstruction};

use super::astlustre::Typ;

// creates an empty CProg
pub fn empty_cprog() -> CProg {
    CProg{state : empty_state(),
        inputs : Vec::new(),
        outputs : Vec::new(),
        local_vars : Vec::new(),
        step : empty_step()}
}

// build a CProg
pub fn build_cprog(state: CState, inputs : Vec<CVar>, outputs : Vec<CVar>, local_vars : Vec<CVar>, step: CStep) -> CProg {
    CProg { state, inputs, outputs, local_vars, step }
}

// creates an empty state
pub fn empty_state() -> CState {
    CState { vars: Vec::new() }
}
/* 
//creates an empty memory
pub fn empty_mem() -> CMemory {
    CMemory { vars: Vec::new() }
}

// builds a memory
pub fn build_mem(vars : Vec<CLocalVar>) -> CMemory {
    CMemory { vars }
}*/

// creates an empty step
pub fn empty_step() -> CStep {
    CStep { body: Vec::new() }
}

// builds a step
pub fn build_step(body : Vec<Cinstruction>) -> CStep {
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

/* 
//build a local cvar with init value None
pub fn build_local_cvar(name : String, vtype : Typ, id : usize) -> CLocalVar {
    CLocalVar { name, id, vtype, init_value: None }
}

//create a local cvar of name "new_i"
pub fn new_localcvar(vtype : Typ, id : usize) -> CLocalVar {
    build_local_cvar(format!("new_{}", id), vtype , id)
}

// builds an Access Var
pub fn build_access_var(var: CVar, depth: i32) -> CAccessVar {
    CAccessVar { var, depth }
}*/

// checks that two cvariables are equal
pub fn eq_var(x: &CVar, y: &CVar) -> bool {
    x.id == y.id && x.name == y.name
}