/* syntactic trees for a subset of C

An end program will look like this
(For now, we consider only programs with one node)

--------------------------------------


struct state {
    // for every variable x
    X,
    PRE_X,
    PRE_PRE_X,
    ...
}

state_t init() {
    initialize the state
}

state_t step(state_t s, inputs)  {
    read inputs
    update step
    write outputs
}

--------------

*/

use crate::astlustre::{self, Typ};

// the C program
// state is the struct
// memory are the local variables made during normalization
// step is the function step
// I WILL REMOVE INPUTS AND OUTPUTS IN FINAL VERSION
pub struct CProg {
    pub state: CState,
    pub memory : CMemory,
    pub inputs : Vec<CVar>,
    pub outputs : Vec<CVar>,
    pub step: CStep,
}

#[derive(Clone)]
pub struct CMemory {
    pub vars: Vec<CLocalVar>
}

// A state is made up of variables
#[derive(Clone)]
pub struct CState {
    pub vars: Vec<CVar>,
}

// A step of the state is a function that takes its inputs
// executes its body and returns the next state
pub struct CStep {
    //inputs : Vec<CVar>,
    pub body: Vec<Cexpr>,
}

// expressions
// CASE(var, true case, false case)
pub enum Cexpr {
    Cconst(CConst),
    Cvar(CAccessVar),
    Cassign(CVar, Box<Cexpr>),
    Cbinop(astlustre::Binop, Box<Cexpr>, Box<Cexpr>),
    Cunop(astlustre::Unop, Box<Cexpr>, Box<Cexpr>),
    Cwhen(Box<Cexpr>, CVar),
    Ccase(CVar, Box<Cexpr>, Box<Cexpr>),
}

// access to a variable in a program
// contains the additional info of which depth needs to be accessed
pub struct CAccessVar {
    var: CVar,
    depth: i32,
}

// variables
// their depth is the depth of their history
// their role is input, output or localvar
#[derive(Clone)]
pub struct CVar {
    pub name: String,
    pub id: usize,
    pub vtype: Typ,
    pub depth: i32,
    pub role: CVarRole,
    pub init_value: Option<CConst>,
}

#[derive(Clone)]
pub struct CLocalVar {
    pub name: String,
    pub id : usize,
    //pub vtype : Typ,
    pub init_value: Option<CConst>,
}

// A Variable can be an input, output or local variable of a program
#[derive(Clone)]
pub enum CVarRole {
    Input,
    Output,
    LocalVar,
}

// constants
#[derive(Clone)]
pub enum CConst {
    CCint(i32),
    CCbool(bool),
    CCfloat(f32),
}

/* Utility functions */

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
pub fn build_local_cvar(name : String, id : usize) -> CLocalVar {
    CLocalVar { name, id, init_value: None }
}

//create a local cvar of name "new_i"
pub fn new_localcvar(id : usize) -> CLocalVar {
    build_local_cvar(format!("new_{}", id), id)
}

// checks that two cvariables are equal
pub fn eq_var(x: &CVar, y: &CVar) -> bool {
    x.id == y.id
}

// builds an Access Var
pub fn build_access_var(var: CVar, depth: i32) -> CAccessVar {
    CAccessVar { var, depth }
}