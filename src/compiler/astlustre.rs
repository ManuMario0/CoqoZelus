/* syntactic trees in Lustre

For now, we consider programs with only one node */

use crate::transpile::{self, CProg, CVar, CVarRole};

// program
pub type LustreProg = Vec<Node>;

// nodes
#[derive(Clone, Debug)]
pub struct Node {
    pub name: String,
    pub input: Vec<Var>,
    pub output: Vec<Var>,
    pub local_vars: Vec<Var>,
    pub body: Vec<Equation>,
}

// equations (body of the functions) var = expression
#[derive(Clone, Debug)]
pub struct Equation {
    pub var: Var,
    pub expression: Expr,
}

// expressions (right-hand side of equations)
// Ecall is a function call on a vector of variables of the size of its input
// but let's not worry about that for now
// MERGE(var, true case, false case)
#[derive(Clone, Debug)]
pub enum Expr {
    Econst(Constant),
    Evar(Var),
    Ebinop(Binop, Box<Expr>, Box<Expr>),
    Eunop(Unop, Box<Expr>),
    Eifthenelse(Box<Expr>, Box<Expr>, Box<Expr>),
    Earrow(Box<Expr>, Box<Expr>),
    Epre(Box<Expr>),
    Efby(Constant, Box<Expr>),
    Ewhen(Box<Expr>, Var),
    Emerge(Var, Box<Expr>, Box<Expr>),
    Ecall(String, Vec<Var>),
}

#[derive(Clone, Debug)]
// types
pub enum Typ {
    Tint,
    Tbool,
    Treal,
    Tvec(u64, Box<Typ>),
}

// variables
#[derive(Clone, Debug)]
pub struct Var {
    pub name: String,
    pub id: usize,
    pub vtype: Typ,
}

// constants
#[derive(Clone, Debug)]
pub enum Constant {
    Cint(i32),
    Cbool(bool),
    Cfloat(f32),
}

// binary operations (excepted fby and arrow)
#[derive(Clone, Debug)]
pub enum Binop {
    Badd,
    Bsub,
    Bmul,
    Bdiv,
    Bleq,
    Bgeq,
    Bl,
    Bg,
    Beq,
    Bneq,
    Band,
    Bor,
    Bxor,
    Bimpl,
}

// unary operations (excepted pre)
#[derive(Clone, Debug)]
pub enum Unop {
    Uneg,
    Unot,
}