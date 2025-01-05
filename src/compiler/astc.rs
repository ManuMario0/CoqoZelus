/* Utility functions for C syntactic tree */

use crate::transpile::{CMemory, CState, CProg, CLocalVar, Cexpr, CStep, CVarRole, CVar, CAccessVar, CConst};

use super::astlustre::Typ;

// build a CProg
pub fn build_cprog(state: CState, memory : CMemory, inputs : Vec<CVar>, outputs : Vec<CVar>, step: CStep) -> CProg {
    CProg { state, memory, step, inputs, outputs }
}

// creates an empty state
pub fn empty_state() -> CState {
    CState { vars: Vec::new() }
}

//creates an empty memory
pub fn empty_mem() -> CMemory {
    CMemory { vars: Vec::new() }
}

// builds a memory
pub fn build_mem(vars : Vec<CLocalVar>) -> CMemory {
    CMemory { vars }
}

// builds a step

pub fn build_step(body : Vec<Cexpr>) -> CStep {
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

//build a local cvar with init value None
pub fn build_local_cvar(name : String, vtype : Typ, id : usize) -> CLocalVar {
    CLocalVar { name, id, vtype, init_value: None }
}

//create a local cvar of name "new_i"
pub fn new_localcvar(vtype : Typ, id : usize) -> CLocalVar {
    build_local_cvar(format!("new_{}", id), vtype , id)
}

// checks that two cvariables are equal
pub fn eq_var(x: &CVar, y: &CVar) -> bool {
    x.id == y.id
}

// builds an Access Var
pub fn build_access_var(var: CVar, depth: i32) -> CAccessVar {
    CAccessVar { var, depth }
}