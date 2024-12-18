use core::panic;
use std::collections::{HashMap, HashSet};

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

fn type_decl(obj: &FileObj, type_tbl : &mut HashMap<String, ASTTypeT>, var_tbl: &mut HashMap<String, ASTTypeT>, d: ASTOneDeclT) -> ASTOneDeclT {
    match d {
        ASTOneDeclT::ASTConstDecl(vec) => {
            let mut res = Vec::with_capacity(vec.len());
            for v in vec {
                match v.val {
                    Some(val) => {
                        let new_val = fill_type_in_expr(obj, type_tbl, var_tbl, val);
                        match v.ttype {
                            Some(t) => {
                                if t == *get_type(&new_val) { // check type
                                    res.push(ASTConstDeclT {
                                        name: v.name,
                                        ttype: Some(t.clone()),
                                        val: Some(new_val)
                                    });
                                    var_tbl.insert(obj.span_str(v.name), t);
                                }
                            }
                            None => { // generate type
                                let new_type = get_type(&new_val).clone();
                                res.push(ASTConstDeclT {
                                    name: v.name,
                                    ttype: Some(get_type(&new_val).clone()),
                                    val: Some(new_val)
                                });
                                var_tbl.insert(obj.span_str(v.name), new_type);
                            }
                        }
                    },
                    None => { // no default set of constants
                        obj.print_error_message(v.name, "This constant does not have any values preset");
                        panic!()
                    }
                }
            }
            ASTOneDeclT::ASTConstDecl(res)
        },
        ASTOneDeclT::ASTTypeDecl(vec) => {
            let mut res = Vec::with_capacity(vec.len());
            for v in vec {
                let ttype_new = fill_type(obj, type_tbl, v.ttype);
                type_tbl.insert(obj.span_str(v.name), ttype_new.clone());
                res.push(ASTTypeDeclT {
                    name: v.name,
                    ttype: ttype_new
                });
            }
            ASTOneDeclT::ASTTypeDecl(res)
        },
        ASTOneDeclT::ASTNodeDecl(mut astnode_decl_t) => {
            let mut params = Vec::with_capacity(astnode_decl_t.params.len());
            for p in astnode_decl_t.params {
                let np = fill_type(obj, type_tbl, p.ttype);
                params.push(ASTVarT{
                    name: p.name,
                    ttype: np.clone()
                });
                var_tbl.insert(obj.span_str(p.name), np);
            }
            let mut returns = Vec::with_capacity(astnode_decl_t.returns.len());
            for r in astnode_decl_t.returns {
                let nr = fill_type(obj, type_tbl, r.ttype);
                returns.push(ASTVarT{
                    name: r.name,
                    ttype: nr.clone()
                });
                var_tbl.insert(obj.span_str(r.name), nr);
            }
            let mut locals = Vec::with_capacity(astnode_decl_t.localVars.len());
            for l in astnode_decl_t.localVars {
                let nl = fill_type(obj, type_tbl, l.ttype);
                locals.push(ASTVarT{
                    name: l.name,
                    ttype: nl.clone()
                });
                var_tbl.insert(obj.span_str(l.name), nl);
            }
            let mut new_body = vec![];
            for e in astnode_decl_t.body {
                match e.lhs {
                    ASTLeftT::ASTAssert() => todo!(),
                    ASTLeftT::ASTLeftItem(vec) => {
                        if vec.len() > 1 {
                            println!("More than one left variable not supported yet");
                            todo!()
                        }
                        match &vec[0] {
                            ASTLeftItemT::ASTVar(span) => {
                                // just check the types
                                let olt = var_tbl.get(&obj.span_str(*span)).cloned();
                                match olt {
                                    Some(lt) => {
                                        let expr = fill_type_in_expr(obj, type_tbl, var_tbl, e.rhs);
                                        if lt == *get_type(&expr) {
                                            new_body.push(ASTEquationT {
                                                lhs: ASTLeftT::ASTLeftItem(vec![ASTLeftItemT::ASTVar(*span)]),
                                                rhs: expr
                                            });
                                        } else {
                                            obj.print_error_message(*span, "Right hand side don't have the correct type");
                                            panic!()
                                        }
                                    },
                                    None => {
                                        obj.print_error_message(*span, "Variable does not exists");
                                        panic!()
                                    },
                                }
                            }
                            ASTLeftItemT::ASTTableElement(astleft_item_t, _)
                            | ASTLeftItemT::ASTTableSlice(astleft_item_t, _) => {
                                println!("Tables are still not handled !!!!!");
                                panic!()
                            }
                        }
                        // for v in vec {
                        //     match v {
                        //         ASTLeftItemT::ASTVar(span) => {
                        //             // just check the types
                        //             let olt = var_tbl.get(&obj.span_str(span)).cloned();
                        //             match olt {
                        //                 Some(lt) => {
                        //                     let expr = fill_type_in_expr(obj, type_tbl, var_tbl, e.rhs);
                        //                     if lt == *get_type(&expr) {
                        //                         new_body.push(ASTEquationT {
                        //                             lhs: ASTLeftT::ASTLeftItem(vec.clone()),
                        //                             rhs: expr
                        //                         });
                        //                     } else {
                        //                         obj.print_error_message(span, "Right hand side don't have the correct type");
                        //                         panic!()
                        //                     }
                        //                 },
                        //                 None => {
                        //                     obj.print_error_message(span, "Variable does not exists");
                        //                     panic!()
                        //                 },
                        //             }
                        //         }
                        //         ASTLeftItemT::ASTTableElement(astleft_item_t, _)
                        //         | ASTLeftItemT::ASTTableSlice(astleft_item_t, _) => {
                        //             println!("Tables are still not handled !!!!!");
                        //             panic!()
                        //         }
                        //     }
                        // }
                    },
                }
            }
            ASTOneDeclT::ASTNodeDecl(ASTNodeDeclT {
                name: astnode_decl_t.name,
                params,
                returns,
                localVars: locals,
                body: new_body
            })
        },
    }
}

fn fill_type_in_expr(obj: &FileObj, type_tbl: &mut HashMap<String, ASTTypeT>, label_tbl: &mut HashMap<String, ASTTypeT>, expr: ASTExprT) -> ASTExprT {
    match expr {
        ASTExprT::ASTConst(span, asttype_t, astconst_t) => {
            match astconst_t {
                ASTConstT::ASTBool(_) => 
                ASTExprT::ASTConst(span, ASTTypeT::ASTBool, astconst_t),
                ASTConstT::ASTInt(_) => 
                ASTExprT::ASTConst(span, ASTTypeT::ASTInt, astconst_t),
                ASTConstT::ASTReal(_) =>
                ASTExprT::ASTConst(span, ASTTypeT::ASTReal, astconst_t),
            }
        },
        ASTExprT::ASTVar(span, _) => {
            match label_tbl.get(&obj.span_str(span)) {
                Some(t) => ASTExprT::ASTVar(span, t.clone()),
                None => {
                    obj.print_error_message(span, "This variable does not exists");
                    panic!()
                },
            }
        },
        ASTExprT::ASTUnop(span, _, astunop_t, astexpr_t) => {
            match astunop_t {
                ASTUnopT::ASTNot => {
                    let expr = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t);
                    let tt = get_type(&expr);
                    if *tt == ASTTypeT::ASTBool {
                        ASTExprT::ASTUnop(span, ASTTypeT::ASTBool, astunop_t, Box::new(expr))
                    } else {
                        obj.print_error_message(span, format!("Typing error : not support only booleans, not {:?}", tt).as_str());
                        panic!()
                    }
                },
                ASTUnopT::ASTMinus => {
                    let expr = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t);
                    let tt = get_type(&expr);
                    if *tt == ASTTypeT::ASTInt {
                        ASTExprT::ASTUnop(span, ASTTypeT::ASTInt, astunop_t, Box::new(expr))
                    } else if *tt == ASTTypeT::ASTReal {
                        ASTExprT::ASTUnop(span, ASTTypeT::ASTReal, astunop_t, Box::new(expr))
                    } else {
                        obj.print_error_message(span, format!("Typing error : not support only integers and reals, not {:?}", tt).as_str());
                        panic!()
                    }
                },
                ASTUnopT::ASTPre 
                | ASTUnopT::ASTCurrent => {
                    let expr = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t);
                    let tt = get_type(&expr);
                    ASTExprT::ASTUnop(span, tt.clone(), astunop_t, Box::new(expr))
                }
                ASTUnopT::ASTInt => {
                    let expr = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t);
                    ASTExprT::ASTUnop(span, ASTTypeT::ASTInt, astunop_t, Box::new(expr))
                },
                ASTUnopT::ASTReal => {
                    let expr = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t);
                    ASTExprT::ASTUnop(span, ASTTypeT::ASTReal, astunop_t, Box::new(expr))
                }
            }
        },
        ASTExprT::ASTBinop(span, _, astbinop_t, astexpr_t1, astexpr_t2) => {
            match astbinop_t {
                ASTBinopT::ASTWhen => {
                    let expr1 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t1);
                    let expr2 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t2);
                    let tt1 = get_type(&expr1);
                    let tt2 = get_type(&expr2);
                    if *tt2 == ASTTypeT::ASTBool {
                        ASTExprT::ASTBinop(span, tt1.clone(), astbinop_t, Box::new(expr1), Box::new(expr2))
                    } else {
                        obj.print_error_message(span, "The type of the clock must be bool");
                        panic!()
                    }
                },
                ASTBinopT::ASTFby
                | ASTBinopT::ASTArrow => {
                    let expr1 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t1);
                    let expr2 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t2);
                    let tt1 = get_type(&expr1);
                    let tt2 = get_type(&expr2);
                    if *tt2 == *tt1 {
                        ASTExprT::ASTBinop(span, tt1.clone(), astbinop_t, Box::new(expr1), Box::new(expr2))
                    } else {
                        obj.print_error_message(span, "The two sides must have same types");
                        panic!()
                    }
                },
                ASTBinopT::ASTAnd
                | ASTBinopT::ASTOr
                | ASTBinopT::ASTXor
                | ASTBinopT::ASTImpl => {
                    let expr1 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t1);
                    let expr2 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t2);
                    let tt1 = get_type(&expr1);
                    let tt2 = get_type(&expr2);
                    if *tt2 == ASTTypeT::ASTBool && *tt1 == ASTTypeT::ASTBool {
                        ASTExprT::ASTBinop(span, ASTTypeT::ASTBool, astbinop_t, Box::new(expr1), Box::new(expr2))
                    } else {
                        obj.print_error_message(span, "The two sides of the operand must be bool");
                        panic!()
                    }
                },
                ASTBinopT::ASTEq
                | ASTBinopT::ASTNeq
                | ASTBinopT::ASTLt
                | ASTBinopT::ASTLe
                | ASTBinopT::ASTGt
                | ASTBinopT::ASTGe => {
                    let expr1 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t1);
                    let expr2 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t2);
                    let tt1 = get_type(&expr1);
                    let tt2 = get_type(&expr2);
                    if *tt2 == *tt1 {
                        ASTExprT::ASTBinop(span, ASTTypeT::ASTBool, astbinop_t, Box::new(expr1), Box::new(expr2))
                    } else {
                        obj.print_error_message(span, "The two sides must have same types");
                        panic!()
                    }
                },
                ASTBinopT::ASTDiv
                | ASTBinopT::ASTMod
                | ASTBinopT::ASTSub
                | ASTBinopT::ASTAdd
                | ASTBinopT::ASTMul => {
                    let expr1 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t1);
                    let expr2 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t2);
                    let tt1 = get_type(&expr1);
                    let tt2 = get_type(&expr2);
                    if *tt2 == *tt1 {
                        ASTExprT::ASTBinop(span, tt1.clone(), astbinop_t, Box::new(expr1), Box::new(expr2))
                    } else {
                        obj.print_error_message(span, "The two sides must have same types");
                        panic!()
                    }
                },
                ASTBinopT::ASTConcat => {
                    obj.print_error_message(span, "Tables are not implemented yet");
                    todo!()
                }
            }
        }
        ASTExprT::ASTIfThenElse(span, _, astexpr_t, astexpr_t1, astexpr_t2) => {
            let expr = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t);
            let expr1 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t1);
            let expr2 = fill_type_in_expr(obj, type_tbl, label_tbl, *astexpr_t2);
            let tt = get_type(&expr);
            let tt1 = get_type(&expr1);
            let tt2 = get_type(&expr2);
            if *tt == ASTTypeT::ASTBool {
                if *tt2 == *tt1 {
                    ASTExprT::ASTIfThenElse(span, tt1.clone(), Box::new(expr), Box::new(expr1), Box::new(expr2))
                } else {
                    obj.print_error_message(span, "Then and else sides must have same type");
                    panic!()
                }
            } else {
                obj.print_error_message(span, "The condition must be a boolean value");
                panic!()
            }
            
        },
        ASTExprT::ASTFuncCall(span, asttype_t) => {
            obj.print_error_message(span, "Function call not treated yet, sorry :(");
            panic!()
        },
        ASTExprT::ASTVec(span, asttype_t, vec) => {
            obj.print_error_message(span, "Tables not treated yet, sorry :(");
            panic!()
        },
        ASTExprT::ASTPow(span, asttype_t, astexpr_t, astexpr_t1) => {
            obj.print_error_message(span, "Tables not treated yet, sorry :(");
            panic!()
        },
        ASTExprT::ASTGetElement(span, asttype_t, astexpr_t, astexpr_t1) => {
            obj.print_error_message(span, "Tables not treated yet, sorry :(");
            panic!()
        },
        ASTExprT::ASTGetSlice(span, asttype_t, astexpr_t, astselect_t) => {
            obj.print_error_message(span, "Tables not treated yet, sorry :(");
            panic!()
        },
        ASTExprT::ASTList(span, asttype_t, vec) => {
            obj.print_error_message(span, "Lists not treated yet, sorry :(");
            panic!()
        },
        ASTExprT::ASTMerge(span, asttype_t, span1, astexpr_t, astexpr_t1) => {
            obj.print_error_message(span, "Tables not treated yet, sorry :(");
            panic!()
        },
    }
}

fn fill_type(obj: &FileObj, type_tbl : &mut HashMap<String, ASTTypeT>, t: ASTTypeT) -> ASTTypeT {
    match t {
        ASTTypeT::ASTNone
        | ASTTypeT::ASTBool
        | ASTTypeT::ASTInt
        | ASTTypeT::ASTReal => t,
        ASTTypeT::ASTLabel(l) => {
            match type_tbl.get(&obj.span_str(l)) {
                Some(tt) => tt.clone(),
                None => {
                    obj.print_error_message(l, "Label does not exists");
                    panic!()
                },
            }
        }
        ASTTypeT::ASTVec(astexpr_t, asttype_t) => todo!(),
    }
}

fn get_type(e: &ASTExprT) -> &ASTTypeT {
    match e {
        ASTExprT::ASTConst(_, asttype_t, _) => &asttype_t,
        ASTExprT::ASTVar(_, asttype_t) => &asttype_t,
        ASTExprT::ASTUnop(_, asttype_t, _, _) => &asttype_t,
        ASTExprT::ASTBinop(_, asttype_t, _, _, _) => &asttype_t,
        ASTExprT::ASTIfThenElse(_, asttype_t, _, _, _) => &asttype_t,
        ASTExprT::ASTFuncCall(_, asttype_t) => &asttype_t,
        ASTExprT::ASTVec(_, asttype_t, _) => &asttype_t,
        ASTExprT::ASTPow(_, asttype_t, _, _) => &asttype_t,
        ASTExprT::ASTGetElement(_, asttype_t, _, _) => &asttype_t,
        ASTExprT::ASTGetSlice(_, asttype_t, _, _) => &asttype_t,
        ASTExprT::ASTList(_, asttype_t, _) => &asttype_t,
        ASTExprT::ASTMerge(_, asttype_t, _, _, _) => &asttype_t,
    }
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
            let name = obj.span_str(span);
            match const_tbl.get(&name) {
                Some(res) => res.clone(),
                None => match var_tbl.get(&name) {
                    Some(_) => ASTExprT::ASTVar(span, ASTTypeT::ASTNone),
                    None => {
                        let ((line, col), _) = obj.line_col(span);
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
