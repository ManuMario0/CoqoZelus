use super::super::parser::*;
use core::panic;
use std::collections::{HashMap, HashSet};

pub fn remove_const(
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
                var_tbl.insert(p.name.clone().get_name(obj));
            }
            for r in &astnode_decl_t.returns {
                var_tbl.insert(r.name.clone().get_name(obj));
            }
            for l in &astnode_decl_t.localVars {
                var_tbl.insert(l.name.clone().get_name(obj));
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
                                    let name = span.clone().get_name(obj);
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
                                            let ((line, col), _) =
                                                obj.line_col(span.clone().get_span());
                                            println!("{name} : this symbol does not exists : line {line} col {col}");
                                            panic!()
                                        }
                                    }
                                }
                                ASTLeftItemT::ASTTableElement(_, _)
                                | ASTLeftItemT::ASTTableSlice(_, _) => {
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
        ASTExprT::ASTConst(s, t, astconst_t) => ASTExprT::ASTConst(s, t, astconst_t),
        ASTExprT::ASTVar(span, _) => {
            let name = span.clone().get_name(obj);
            match const_tbl.get(&name) {
                Some(res) => res.clone(),
                None => match var_tbl.get(&name) {
                    Some(_) => ASTExprT::ASTVar(span, ASTTypeT::ASTNone),
                    None => {
                        let ((line, col), _) = obj.line_col(span.get_span());
                        println!("{name} : this symbol does not exists : line {line} col {col}");
                        panic!()
                    }
                },
            }
        }
        ASTExprT::ASTUnop(s, t, op, astexpr_t) => ASTExprT::ASTUnop(
            s,
            t,
            op,
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
        ),
        ASTExprT::ASTBinop(s, t, op, ast1, ast2) => ASTExprT::ASTBinop(
            s,
            t,
            op,
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *ast1)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *ast2)),
        ),
        ASTExprT::ASTIfThenElse(s, t, astexpr_t, astexpr_t1, astexpr_t2) => {
            ASTExprT::ASTIfThenElse(
                s,
                t,
                Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t)),
                Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t1)),
                Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *astexpr_t2)),
            )
        }
        ASTExprT::ASTFuncCall(s, _) => {
            obj.print_error_message(s, "Function call not supported yet");
            todo!()
        }
        ASTExprT::ASTVec(s, _, _) => {
            obj.print_error_message(s, "Tables not supported yet");
            todo!()
        }
        ASTExprT::ASTPow(s, _, _, _) => {
            obj.print_error_message(s, "^ operation not supported yet");
            todo!()
        }
        ASTExprT::ASTGetElement(s, _, astexpr_t, astexpr_t1) => {
            obj.print_error_message(s, "Tables not supported yet");
            todo!()
        }
        ASTExprT::ASTGetSlice(s, _, astexpr_t, astselect_t) => {
            obj.print_error_message(s, "Tables not supported yet");
            todo!()
        }
        ASTExprT::ASTList(s, _, vec) => {
            obj.print_error_message(s, "List of object not supported yet");
            todo!()
        }
        ASTExprT::ASTMerge(s, t, name, ast1, ast2) => ASTExprT::ASTMerge(
            s,
            t,
            name,
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *ast1)),
            Box::new(remove_const_from_expr(obj, const_tbl, var_tbl, *ast2)),
        ),
    }
}
