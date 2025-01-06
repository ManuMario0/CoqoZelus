use core::panic;
use std::{
    collections::{HashMap, HashSet},
    u64, vec,
};

mod flattener;
mod typing;
use flattener::*;
use typing::*;

use crate::compiler::astlustre::{self, Unop};

/// The point of this mod will be, in a first approximation,
/// to not type but simply extend constants and remove types.
/// We just want to come as close a possible to the simple
/// AST tree used at compilation, without checking anykind of possible
/// typing issues.
use super::parser::*;

// Lets type this thingamabob since I cannot do anything alse rn
pub fn type_ast<'a>(mut obj: FileObj<'a, 'a>) -> Result<FileObj<'a, 'a>, ()> {
    let mut ast = obj.get_ast().clone();
    let mut type_tbl = HashMap::new();
    let mut val_tbl = HashMap::new();
    let mut res = vec![];
    for v in ast {
        res.push(type_decl(&obj, &mut type_tbl, &mut val_tbl, v));
    }
    *obj.get_mut_ast() = res;
    Ok(obj)
}

pub fn flatten_const<'a>(mut obj: FileObj<'a, 'a>) -> Result<FileObj<'a, 'a>, ()> {
    let mut ast = obj.get_ast().clone();
    let mut const_tbl = HashMap::new();
    let mut limit = ast.len();
    let mut i = 0;
    while i < limit {
        match remove_const(&obj, &mut const_tbl, ast[i].to_owned()) {
            Some(v) => {
                ast[i] = v;
                i += 1;
            }
            None => {
                let _ = ast.remove(i);
                limit = limit - 1;
            }
        }
    }
    *obj.get_mut_ast() = ast;
    Ok(obj)
}

pub fn simplify_ast<'a>(mut obj: FileObj<'a, 'a>) -> Result<FileObj<'a, 'a>, ()> {
    let mut ast = obj.get_ast().clone();
    let mut res = vec![];
    for a in ast {
        res.push(simplify_decl(a));
    }
    *obj.get_mut_ast() = res;
    Ok(obj)
}

pub fn translate<'a>(obj: &FileObj<'a, 'a>) -> Result<astlustre::LustreProg, ()> {
    // now, everything should be type checked and faltten so we can just
    // crash whenever we have something unexpecterd as it should not happened

    let ast = obj.get_ast();
    let mut res = Vec::with_capacity(ast.len());

    let mut id = 0;
    let mut id_map = HashMap::new();

    for a in ast {
        match a {
            ASTOneDeclT::ASTNodeDecl(n) => {
                let node = astlustre::Node {
                    name: obj.span_str(n.name),
                    input: n
                        .params
                        .iter()
                        .map(|v| translate_var(obj, v, &mut id, &mut id_map))
                        .collect(),
                    output: n
                        .returns
                        .iter()
                        .map(|v| translate_var(obj, v, &mut id, &mut id_map))
                        .collect(),
                    local_vars: n
                        .localVars
                        .iter()
                        .map(|v| translate_var(obj, v, &mut id, &mut id_map))
                        .collect(),
                    body: n
                        .body
                        .iter()
                        .map(|e| translate_equation(obj, e, &id_map))
                        .collect(),
                };
                res.push(node);
            }
            _ => return Err(()),
        }
    }

    Ok(res)
}

fn translate_var<'a>(
    obj: &FileObj<'a, 'a>,
    v: &ASTVarT,
    id: &mut usize,
    map: &mut HashMap<String, astlustre::Var>,
) -> astlustre::Var {
    *id += 1;
    let name = obj.span_str(v.name);
    let var = astlustre::Var {
        name: name.clone(),
        id: *id,
        vtype: translate_type(&v.ttype),
    };
    map.insert(name, var.clone());
    var
}

fn translate_type(t: &ASTTypeT) -> astlustre::Typ {
    match t {
        ASTTypeT::ASTNone => panic!(),
        ASTTypeT::ASTBool => astlustre::Typ::Tbool,
        ASTTypeT::ASTInt => astlustre::Typ::Tint,
        ASTTypeT::ASTReal => astlustre::Typ::Treal,
        ASTTypeT::ASTLabel(_) => panic!(),
        ASTTypeT::ASTVec(_, _) => panic!(),
    }
}

fn translate_equation<'a>(
    obj: &FileObj<'a, 'a>,
    e: &ASTEquationT,
    map: &HashMap<String, astlustre::Var>,
) -> astlustre::Equation {
    // translate lhs
    let var_name = match &e.lhs {
        ASTLeftT::ASTAssert() => todo!(),
        ASTLeftT::ASTLeftItem(vec) => match vec[0] {
            ASTLeftItemT::ASTVar(span) => obj.span_str(span),
            _ => todo!(),
        },
    };
    let var = map.get(&var_name).unwrap();

    // now rhs
    let expression = translate_expr(obj, &e.rhs, map);

    astlustre::Equation {
        var: var.clone(),
        expression,
    }
}

fn translate_expr<'a>(
    obj: &FileObj<'a, 'a>,
    e: &ASTExprT,
    map: &HashMap<String, astlustre::Var>,
) -> astlustre::Expr {
    match e {
        ASTExprT::ASTConst(_, _, astconst_t) => match astconst_t {
            ASTConstT::ASTBool(b) => astlustre::Expr::Econst(astlustre::Constant::Cbool(*b)),
            ASTConstT::ASTInt(i) => astlustre::Expr::Econst(astlustre::Constant::Cint(*i as i32)),
            ASTConstT::ASTReal(r) => {
                astlustre::Expr::Econst(astlustre::Constant::Cfloat(*r as f32))
            }
        },
        ASTExprT::ASTVar(s, _) => {
            let var_name = obj.span_str(*s);
            astlustre::Expr::Evar(map.get(&var_name).unwrap().clone())
        }
        ASTExprT::ASTUnop(_, _, unop, astexpr_t) => match unop {
            ASTUnopT::ASTNot => {
                astlustre::Expr::Eunop(Unop::Unot, Box::new(translate_expr(obj, &astexpr_t, map)))
            }
            ASTUnopT::ASTMinus => {
                astlustre::Expr::Eunop(Unop::Uneg, Box::new(translate_expr(obj, &astexpr_t, map)))
            }
            ASTUnopT::ASTPre => {
                astlustre::Expr::Epre(Box::new(translate_expr(obj, &astexpr_t, map)))
            }
            ASTUnopT::ASTCurrent => todo!(),
            ASTUnopT::ASTInt => todo!(),
            ASTUnopT::ASTReal => todo!(),
        },
        ASTExprT::ASTBinop(_, _, binop, astexpr_t, astexpr_t1) => match binop {
            ASTBinopT::ASTWhen => {
                let var = match **astexpr_t1 {
                    ASTExprT::ASTVar(span, _) => map.get(&obj.span_str(span)).unwrap(),
                    _ => panic!(),
                };
                astlustre::Expr::Ewhen(Box::new(translate_expr(obj, astexpr_t, map)), var.clone())
            }
            ASTBinopT::ASTFby => match **astexpr_t {
                ASTExprT::ASTConst(_, _, ASTConstT::ASTBool(b)) => astlustre::Expr::Efby(
                    astlustre::Constant::Cbool(b),
                    Box::new(translate_expr(obj, astexpr_t1, map)),
                ),
                ASTExprT::ASTConst(_, _, ASTConstT::ASTInt(i)) => astlustre::Expr::Efby(
                    astlustre::Constant::Cint(i as i32),
                    Box::new(translate_expr(obj, astexpr_t1, map)),
                ),
                ASTExprT::ASTConst(_, _, ASTConstT::ASTReal(r)) => astlustre::Expr::Efby(
                    astlustre::Constant::Cfloat(r as f32),
                    Box::new(translate_expr(obj, astexpr_t1, map)),
                ),
                _ => panic!(),
            },
            ASTBinopT::ASTArrow => astlustre::Expr::Earrow(
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTAnd => astlustre::Expr::Ebinop(
                astlustre::Binop::Band,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTOr => astlustre::Expr::Ebinop(
                astlustre::Binop::Bor,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTXor => astlustre::Expr::Ebinop(
                astlustre::Binop::Bxor,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTImpl => astlustre::Expr::Ebinop(
                astlustre::Binop::Bimpl,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTEq => astlustre::Expr::Ebinop(
                astlustre::Binop::Beq,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTNeq => astlustre::Expr::Ebinop(
                astlustre::Binop::Bneq,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTLt => astlustre::Expr::Ebinop(
                astlustre::Binop::Bl,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTLe => astlustre::Expr::Ebinop(
                astlustre::Binop::Bleq,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTGt => astlustre::Expr::Ebinop(
                astlustre::Binop::Bg,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTGe => astlustre::Expr::Ebinop(
                astlustre::Binop::Bgeq,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTDiv => astlustre::Expr::Ebinop(
                astlustre::Binop::Bdiv,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTMod => todo!(),
            ASTBinopT::ASTSub => astlustre::Expr::Ebinop(
                astlustre::Binop::Bsub,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTAdd => astlustre::Expr::Ebinop(
                astlustre::Binop::Bmul,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTMul => astlustre::Expr::Ebinop(
                astlustre::Binop::Bmul,
                Box::new(translate_expr(obj, astexpr_t, map)),
                Box::new(translate_expr(obj, astexpr_t1, map)),
            ),
            ASTBinopT::ASTConcat => todo!(),
        },
        ASTExprT::ASTIfThenElse(_, _, astexpr_t, astexpr_t1, astexpr_t2) => todo!(),
        ASTExprT::ASTFuncCall(_, _) => todo!(),
        ASTExprT::ASTVec(_, _, vec) => todo!(),
        ASTExprT::ASTPow(_, _, astexpr_t, astexpr_t1) => todo!(),
        ASTExprT::ASTGetElement(_, _, astexpr_t, astexpr_t1) => todo!(),
        ASTExprT::ASTGetSlice(_, _, astexpr_t, astselect_t) => todo!(),
        ASTExprT::ASTList(_, _, vec) => todo!(),
        ASTExprT::ASTMerge(_, _, astexpr_t, astexpr_t1, astexpr_t2) => {
            todo!() /*astlustre::Expr::Emerge(
                        astlustre::Binop::Band,
                        Box::new(translate_expr(obj, astexpr_t, map)),
                        Box::new(translate_expr(obj, astexpr_t1, map))
                    ),*/
        }
    }
}

fn simplify_decl(a: ASTOneDeclT) -> ASTOneDeclT {
    match a {
        ASTOneDeclT::ASTNodeDecl(astnode_decl_t) => {
            let params = astnode_decl_t.params;
            let returns = astnode_decl_t.returns;
            let locals = astnode_decl_t.localVars;
            let mut res = Vec::with_capacity(astnode_decl_t.body.len());
            for e in astnode_decl_t.body {
                res.push(ASTEquationT {
                    lhs: e.lhs,
                    rhs: simplify_expr(e.rhs),
                });
            }
            ASTOneDeclT::ASTNodeDecl(ASTNodeDeclT {
                name: astnode_decl_t.name,
                params,
                returns,
                localVars: locals,
                body: res,
            })
        }
        _ => a,
    }
}

fn simplify_expr(expr: ASTExprT) -> ASTExprT {
    match expr {
        ASTExprT::ASTConst(_, _, _) | ASTExprT::ASTVar(_, _) | ASTExprT::ASTFuncCall(_, _) => expr,
        ASTExprT::ASTUnop(a, b, ASTUnopT::ASTPre, astexpr_t) => {
            let tmp = simplify_expr(*astexpr_t);
            let cst = constant_gen_from_type(&b);
            // I don't know what to use for bottom as I still don't know the type (and don't want to be bothered with it)
            // so I simply use the right side of the expression as first value lol
            ASTExprT::ASTBinop(
                a,
                b.clone(),
                ASTBinopT::ASTFby,
                Box::new(ASTExprT::ASTConst(a, b, cst)),
                Box::new(tmp),
            )
        }
        ASTExprT::ASTUnop(a, b, c, astexpr_t) => {
            ASTExprT::ASTUnop(a, b, c, Box::new(simplify_expr(*astexpr_t)))
        }
        ASTExprT::ASTBinop(a, b, ASTBinopT::ASTArrow, expr1, expr2) => {
            simplify_expr(ASTExprT::ASTIfThenElse(
                a,
                b.clone(),
                Box::new(ASTExprT::ASTBinop(
                    a,
                    b,
                    ASTBinopT::ASTFby,
                    Box::new(ASTExprT::ASTConst(
                        a,
                        ASTTypeT::ASTBool,
                        ASTConstT::ASTBool(true),
                    )),
                    Box::new(ASTExprT::ASTConst(
                        a,
                        ASTTypeT::ASTBool,
                        ASTConstT::ASTBool(false),
                    )),
                )),
                Box::new(simplify_expr(*expr1)),
                Box::new(simplify_expr(*expr2)),
            ))
        }
        ASTExprT::ASTBinop(a, b, _any, expr1, expr2) => ASTExprT::ASTBinop(
            a,
            b,
            _any,
            Box::new(simplify_expr(*expr1)),
            Box::new(simplify_expr(*expr2)),
        ),
        ASTExprT::ASTIfThenElse(a, b, astexpr_t, astexpr_t1, astexpr_t2) => ASTExprT::ASTMerge(
            a,
            b.clone(),
            astexpr_t.clone(),
            Box::new(ASTExprT::ASTBinop(
                a,
                b.clone(),
                ASTBinopT::ASTWhen,
                astexpr_t1,
                astexpr_t.clone(),
            )),
            Box::new(ASTExprT::ASTBinop(
                a,
                b.clone(),
                ASTBinopT::ASTWhen,
                astexpr_t2,
                Box::new(ASTExprT::ASTUnop(a, b, ASTUnopT::ASTNot, astexpr_t)),
            )),
        ),
        ASTExprT::ASTVec(_, _, _) => todo!(),
        ASTExprT::ASTPow(_, _, _, _) => todo!(),
        ASTExprT::ASTGetElement(_, _, _, _) => todo!(),
        ASTExprT::ASTGetSlice(_, _, _, _) => todo!(),
        ASTExprT::ASTList(_, _, _) => todo!(),
        ASTExprT::ASTMerge(a, b, c, astexpr_t, astexpr_t1) => ASTExprT::ASTMerge(
            a,
            b,
            Box::new(simplify_expr(*c)),
            Box::new(simplify_expr(*astexpr_t)),
            Box::new(simplify_expr(*astexpr_t1)),
        ),
    }
}

fn constant_gen_from_type(t: &ASTTypeT) -> ASTConstT {
    match t {
        ASTTypeT::ASTNone => panic!("Please type check the ast before running this instruction"),
        ASTTypeT::ASTBool => ASTConstT::ASTBool(false),
        ASTTypeT::ASTInt => ASTConstT::ASTInt(0),
        ASTTypeT::ASTReal => ASTConstT::ASTReal(0.),
        ASTTypeT::ASTLabel(_) => {
            panic!("Please type check the ast before running this instruction")
        }
        ASTTypeT::ASTVec(astexpr_t, asttype_t) => todo!(),
    }
}
