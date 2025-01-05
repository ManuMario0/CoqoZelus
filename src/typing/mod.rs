use std::{collections::{HashMap, HashSet}, vec};

mod typing;
mod flattener;
use typing::*;
use flattener::*;

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

pub fn simplify_decl(a: ASTOneDeclT) -> ASTOneDeclT {
    match a {
        ASTOneDeclT::ASTNodeDecl(astnode_decl_t) => {
            let params = astnode_decl_t.params;
            let returns = astnode_decl_t.returns;
            let locals = astnode_decl_t.localVars;
            let mut res = Vec::with_capacity(astnode_decl_t.body.len());
            for e in astnode_decl_t.body {
                res.push(ASTEquationT {
                    lhs: e.lhs,
                    rhs: simplify_expr(e.rhs)
                });
            }
            ASTOneDeclT::ASTNodeDecl(ASTNodeDeclT {
                name: astnode_decl_t.name,
                params,
                returns,
                localVars: locals,
                body: res
            })
        },
        _ => a,
    }
}

fn simplify_expr(expr: ASTExprT) -> ASTExprT {
    match expr {
        ASTExprT::ASTConst(_, _, _)
        | ASTExprT::ASTVar(_, _)
        | ASTExprT::ASTFuncCall(_, _) => expr,
        ASTExprT::ASTUnop(a, b, ASTUnopT::ASTPre, astexpr_t) =>
            {
                let tmp = simplify_expr(*astexpr_t);
                // I don't know what to use for bottom as I still don't know the type (and don't want to be bothered with it)
                // so I simply use the right side of the expression as first value lol
                ASTExprT::ASTBinop(a, b, ASTBinopT::ASTFby, Box::new(tmp.clone()), Box::new(tmp))
            }
        ,
        ASTExprT::ASTUnop(a, b, c, astexpr_t) => 
            ASTExprT::ASTUnop(a, b, c, Box::new(simplify_expr(*astexpr_t))),
        ASTExprT::ASTBinop(a, b, ASTBinopT::ASTArrow, expr1, expr2) =>
        simplify_expr(ASTExprT::ASTIfThenElse(
                a,
                b.clone(),
                Box::new(
                    ASTExprT::ASTBinop(a, b, 
                        ASTBinopT::ASTFby, 
                        Box::new(ASTExprT::ASTConst(a, ASTTypeT::ASTBool, ASTConstT::ASTBool(true))),
                        Box::new(ASTExprT::ASTConst(a, ASTTypeT::ASTBool, ASTConstT::ASTBool(false))),
                    )),
                Box::new(simplify_expr(*expr1)),
                Box::new(simplify_expr(*expr2))
            )
        ),
        ASTExprT::ASTBinop(a, b, _any, expr1, expr2) => 
            ASTExprT::ASTBinop(a, b, _any, Box::new(simplify_expr(*expr1)), Box::new(simplify_expr(*expr2))),
        ASTExprT::ASTIfThenElse(a, b, astexpr_t, astexpr_t1, astexpr_t2) => {
            ASTExprT::ASTMerge(a, b.clone(), astexpr_t.clone(), 
                Box::new(ASTExprT::ASTBinop(a, b.clone(), ASTBinopT::ASTWhen, astexpr_t1, astexpr_t.clone())), 
                Box::new(ASTExprT::ASTBinop(a, b.clone(), ASTBinopT::ASTWhen, astexpr_t2, Box::new(ASTExprT::ASTUnop(a, b, ASTUnopT::ASTNot, astexpr_t))))
            )
        }
        ASTExprT::ASTVec(_, _, _) => todo!(),
        ASTExprT::ASTPow(_, _, _, _) => todo!(),
        ASTExprT::ASTGetElement(_, _, _, _) => todo!(),
        ASTExprT::ASTGetSlice(_, _, _, _) => todo!(),
        ASTExprT::ASTList(_, _, _) => todo!(),
        ASTExprT::ASTMerge(a, b, c, astexpr_t, astexpr_t1) => 
            ASTExprT::ASTMerge(a, b, 
                Box::new(simplify_expr(*c)), 
                Box::new(simplify_expr(*astexpr_t)), 
                Box::new(simplify_expr(*astexpr_t1))
            ),
    }
}