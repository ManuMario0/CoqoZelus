use std::collections::{HashMap, HashSet};

/// The point of this mod will be, in a first approximation,
/// to not type but simply extend constants and remove types.
/// We just want to come as close a possible to the simple
/// AST tree used at compilation, without checking anykind of possible
/// typing issues.
use super::parser::*;

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

fn remove_const(
    obj: &FileObj,
    const_tbl: &mut HashMap<String, ASTExprT>,
    v: ASTOneDeclT,
) -> Option<ASTOneDeclT> {
    match v {
        ASTOneDeclT::ASTConstDecl(vec) => {
            for c in vec {
                match c.val {
                    Some(val) => {
                        // we add an empty var table so that we don't have to create two distinct
                        // but very similay checking funcs
                        let var_tbl = HashSet::new();
                        let expr = remove_const_from_expr(obj, const_tbl, &var_tbl, val);
                        const_tbl.insert(obj.span_str(c.name), expr);
                    }
                    None => {
                        let ((line, col), _) = obj.line_col(c.name);
                        println!("Constant not assigned: line {line}, col {col}");
                        panic!()
                    }
                }
            }
            None
        }
        ASTOneDeclT::ASTTypeDecl(_) => None, // types are simply ignored for now
        ASTOneDeclT::ASTNodeDecl(mut astnode_decl_t) => {
            let mut var_tbl = HashSet::new();
            for p in &astnode_decl_t.params {
                var_tbl.insert(obj.span_str(p.name));
            }
            for r in &astnode_decl_t.returns {
                var_tbl.insert(obj.span_str(r.name));
            }
            for l in &astnode_decl_t.localVars {
                var_tbl.insert(obj.span_str(l.name));
            }
            let mut new_body = vec![];
            for e in astnode_decl_t.body {
                match &e.lhs {
                    ASTLeftT::ASTAssert() => todo!(),
                    ASTLeftT::ASTLeftItem(vec) => {
                        if vec.len() > 1 {
                            println!("More than one left variable not supported yet");
                            todo!()
                        }
                        for v in vec {
                            match v {
                                ASTLeftItemT::ASTVar(span) => {
                                    let name = obj.span_str(*span);
                                    match var_tbl.get(&name) {
                                        Some(_) => {
                                            new_body.push(ASTEquationT {
                                                lhs: e.lhs.clone(),
                                                rhs: remove_const_from_expr(
                                                    obj,
                                                    &const_tbl,
                                                    &var_tbl,
                                                    e.rhs.clone(),
                                                ),
                                            });
                                        }
                                        None => {
                                            let ((line, col), _) = obj.line_col(*span);
                                            println!("{name} : this symbol does not exists : line {line} col {col}");
                                            panic!()
                                        }
                                    }
                                }
                                ASTLeftItemT::ASTTableElement(astleft_item_t, _)
                                | ASTLeftItemT::ASTTableSlice(astleft_item_t, _) => {
                                    println!("Tables not supported yet");
                                    todo!()
                                }
                            }
                        }
                    }
                }
            }
            astnode_decl_t.body = new_body;
            Some(ASTOneDeclT::ASTNodeDecl(astnode_decl_t))
        }
    }
}

fn remove_const_from_expr(
    obj: &FileObj,
    const_tbl: &HashMap<String, ASTExprT>,
    var_tbl: &HashSet<String>,
    v: ASTExprT,
) -> ASTExprT {
    match v {
        ASTExprT::ASTConst(astconst_t) => ASTExprT::ASTConst(astconst_t),
        ASTExprT::ASTVar(span) => {
            let name = obj.span_str(span);
            match const_tbl.get(&name) {
                Some(res) => res.clone(),
                None => match var_tbl.get(&name) {
                    Some(_) => ASTExprT::ASTVar(span),
                    None => {
                        let ((line, col), _) = obj.line_col(span);
                        println!("{name} : this symbol does not exists : line {line} col {col}");
                        panic!()
                    }
                },
            }
        }
        ASTExprT::ASTNot(astexpr_t) => ASTExprT::ASTNot(Box::new(remove_const_from_expr(
            obj, const_tbl, var_tbl, *astexpr_t,
        ))),
        ASTExprT::ASTMinus(astexpr_t) => ASTExprT::ASTMinus(Box::new(remove_const_from_expr(
            obj, const_tbl, var_tbl, *astexpr_t,
        ))),
        ASTExprT::ASTPre(astexpr_t) => ASTExprT::ASTPre(Box::new(remove_const_from_expr(
            obj, const_tbl, var_tbl, *astexpr_t,
        ))),
        ASTExprT::ASTCurrent(astexpr_t) => ASTExprT::ASTCurrent(Box::new(remove_const_from_expr(
            obj, const_tbl, var_tbl, *astexpr_t,
        ))),
        ASTExprT::ASTInt(astexpr_t) => ASTExprT::ASTInt(Box::new(remove_const_from_expr(
            obj, const_tbl, var_tbl, *astexpr_t,
        ))),
        ASTExprT::ASTReal(astexpr_t) => ASTExprT::ASTReal(Box::new(remove_const_from_expr(
            obj, const_tbl, var_tbl, *astexpr_t,
        ))),
        ASTExprT::ASTWhen(astexpr_t, astclock_expr_t) => match astclock_expr_t {
            ASTClockExprT::ASTPosClock(span) | ASTClockExprT::ASTNegClock(span) => {
                let name = obj.span_str(span);
                match const_tbl.get(&name) {
                    Some(res) => {
                        let ((line, col), _) = obj.line_col(span);
                        println!(
                            "{name} should be a variable, not a constant : line {line} col {col}"
                        );
                        panic!()
                    }
                    None => match var_tbl.get(&name) {
                        Some(_) => ASTExprT::ASTWhen(
                            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
                            astclock_expr_t,
                        ),
                        None => {
                            let ((line, col), _) = obj.line_col(span);
                            println!(
                                "{name} : this symbol does not exists : line {line} col {col}"
                            );
                            panic!()
                        }
                    },
                }
            }
        },
        ASTExprT::ASTFby(astexpr_t, astexpr_t1) => ASTExprT::ASTFby(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTArrow(astexpr_t, astexpr_t1) => ASTExprT::ASTArrow(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTAnd(astexpr_t, astexpr_t1) => ASTExprT::ASTAnd(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTOr(astexpr_t, astexpr_t1) => ASTExprT::ASTOr(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTXor(astexpr_t, astexpr_t1) => ASTExprT::ASTXor(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTImpl(astexpr_t, astexpr_t1) => ASTExprT::ASTImpl(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTEq(astexpr_t, astexpr_t1) => ASTExprT::ASTEq(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTNeq(astexpr_t, astexpr_t1) => ASTExprT::ASTNeq(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTLt(astexpr_t, astexpr_t1) => ASTExprT::ASTLt(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTLe(astexpr_t, astexpr_t1) => ASTExprT::ASTLe(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTGt(astexpr_t, astexpr_t1) => ASTExprT::ASTGt(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTGe(astexpr_t, astexpr_t1) => ASTExprT::ASTGe(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTDiv(astexpr_t, astexpr_t1) => ASTExprT::ASTDiv(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTMod(astexpr_t, astexpr_t1) => ASTExprT::ASTMod(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTSub(astexpr_t, astexpr_t1) => ASTExprT::ASTSub(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTAdd(astexpr_t, astexpr_t1) => ASTExprT::ASTAdd(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTMul(astexpr_t, astexpr_t1) => ASTExprT::ASTMul(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
        ),
        ASTExprT::ASTIfThenElse(astexpr_t, astexpr_t1, astexpr_t2) => ASTExprT::ASTIfThenElse(
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t2)),
        ),
        ASTExprT::ASTFuncCall() => {
            println!("Functions call not supported yet");
            panic!()
        }
        ASTExprT::ASTVec(vec) => {
            println!("Tables not supported yet");
            panic!()
        }
        // ASTExprT::ASTVec(Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t,
        // )),
        // Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t1,
        // ))),
        ASTExprT::ASTPow(astexpr_t, astexpr_t1) => {
            println!("^ operation not supported yet");
            panic!()
        }
        // ASTExprT::ASTPow(Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t,
        // )),
        // Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t1,
        // ))),
        ASTExprT::ASTGetElement(astexpr_t, astexpr_t1) => {
            println!("Tables not supported yet");
            panic!()
        }
        // ASTExprT::ASTGetElement(Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t,
        // )),
        // Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t1,
        // ))),
        ASTExprT::ASTGetSlice(astexpr_t, astselect_t) => {
            println!("Tables not supported yet");
            panic!()
        }
        // ASTExprT::ASTGetSlice(Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t,
        // )),
        // Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t1,
        // ))),
        ASTExprT::ASTConcat(astexpr_t, astexpr_t1) => {
            println!("| opperation not supported");
            panic!()
        }
        // ASTExprT::AST(Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t,
        // )),
        // Box::new(remove_const_from_expr(
        //     obj,
        //     const_tbl,
        //     var_tbl,
        //     *astexpr_t1,
        // ))),
        ASTExprT::ASTList(vec) => {
            println!("List of object not supported yet");
            panic!()
        }
    }
}
