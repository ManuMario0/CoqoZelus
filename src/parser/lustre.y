%start program
%nonassoc 'STEP'
%left ',' '^' '|'
%right 'REAL' 'INT'
%left 'ELSE'
%right 'FBY' '->'
%right '=>'
%left 'OR' 'XOR'
%left 'AND'
%nonassoc '<' '<=' '=' '>=' '>' '<>'
%right 'NOT'
%left '+' '-'
%left '*' '/' 'MOD' 'DIV'
%right 'WHEN'
%right 'PRE' 'CURRENT'
%nonassoc '.' '[' '(' ']' ')'
%%
program -> Result<ASTProgramT, ()>:
    PackBody { $1 }
;

PackBody -> Result<ASTProgramT, ()>:
    OneDecl {
        Ok(vec![$1?])
    }
    | PackBody OneDecl {
        flatten($1, $2)
    }
;

OneDecl -> Result<ASTOneDeclT, ()>:
    ConstDecl {
        Ok(ASTOneDeclT::ASTConstDecl($1?))
    }
    | TypeDecl {
        Ok(ASTOneDeclT::ASTTypeDecl($1?))
    }
    //| ExtNodeDecl
    | NodeDecl {
        Ok(ASTOneDeclT::ASTNodeDecl($1?))
    }
;

NodeDecl -> Result<ASTNodeDeclT, ()>:
    'NODE' Lv6Id Params 'RETURNS' Params 'VAR' VarDeclList ';' Body {
        Ok(ASTNodeDeclT {
            name: $2?,
            params: $3?,
            returns: $5?,
            localVars: $7?,
            body: $9?,
        })
    }
    | 'NODE' Lv6Id Params 'RETURNS' Params Body {
        Ok(ASTNodeDeclT {
            name: $2?,
            params: $3?,
            returns: $5?,
            localVars: vec![],
            body: $6?,
        })
    }
    | 'NODE' Lv6Id Params 'RETURNS' Params ';' 'VAR' VarDeclList ';' Body {
        Ok(ASTNodeDeclT {
            name: $2?,
            params: $3?,
            returns: $5?,
            localVars: $8?,
            body: $10?,
        })
    }
    | 'NODE' Lv6Id Params 'RETURNS' Params ';' Body {
        Ok(ASTNodeDeclT {
            name: $2?,
            params: $3?,
            returns: $5?,
            localVars: vec![],
            body: $7?,
        })
    }
;

Body -> Result<ASTBodyT, ()>:
    'LET' 'TEL' {
        Ok(vec![])
    }
    | 'LET' EquationList 'TEL' {
        $2
    }
;

EquationList -> Result<Vec<ASTEquationT>, ()>:
    Equation {
        Ok(vec![$1?])
    }
    | EquationList Equation {
        flatten($1, $2)
    }
;

Equation -> Result<ASTEquationT, ()>:
    'ASSERT' Expression ';' {
        Ok(ASTEquationT {
            lhs: ASTLeftT::ASTAssert(),
            rhs: $2?
        })
    }
    | Left '=' Expression ';' {
        Ok(ASTEquationT {
            lhs: ASTLeftT::ASTLeftItem($1?),
            rhs: $3?
        })
    }
;

Left -> Result<Vec<ASTLeftItemT>, ()>:
    LeftItemList {
        $1
    }
    | '(' LeftItemList ')' {
        $2
    }
;

LeftItemList -> Result<Vec<ASTLeftItemT>, ()>:
    LeftItem {
        Ok(vec![$1?])
    }
    | LeftItemList LeftItem {
        flatten($1, $2)
    }
;

LeftItem -> Result<ASTLeftItemT, ()>:
    Lv6Id {
        Ok(ASTLeftItemT::ASTVar($1?))
    }
    | LeftItem '[' Expression ']' {
        Ok(ASTLeftItemT::ASTTableElement(Box::new($1?), $3?))
    }
    | LeftItem '[' Select ']' {
        Ok(ASTLeftItemT::ASTTableSlice(Box::new($1?), $3?))
    }
;

Params -> Result<Vec<ASTVarT>, ()>:
    '(' ')' {
        Ok(vec![])
    }
    | '(' VarDeclList ')' {
        Ok($2?)
    }
    | '(' VarDeclList ';' ')' {
        Ok($2?)
    }
;

VarDeclList -> Result<Vec<ASTVarT>, ()>:
    VarDecl {
        Ok($1?)
    }
    | VarDeclList ';' VarDecl {
        Ok([$1?, $3?].concat())
    }
;

VarDecl -> Result<Vec<ASTVarT>, ()>:
    Lv6IdList ':' Type {
        let mut res = vec![];
        for v in $1? {
            res.push(
                ASTVarT {
                    name: v,
                    ttype: $3.clone()?
                }
            )
        }
        Ok(res)
    }
;

TypeDecl -> Result<Vec<ASTTypeDeclT>, ()>:
    'TYPE' TypeDeclList { $2 }
;

TypeDeclList -> Result<Vec<ASTTypeDeclT>, ()>:
    OneTypeDecl ';' { Ok(vec![$1?]) }
    | TypeDeclList OneTypeDecl ';' {
        flatten($1, $2)
    }
;

OneTypeDecl -> Result<ASTTypeDeclT, ()>:
    Lv6Id '=' Type {
        Ok(ASTTypeDeclT {
            name: $1?,
            ttype: $3?
        })
    }
;

ConstDecl -> Result<Vec<ASTConstDeclT>, ()>:
    'CONST' ConstDeclList { $2 }
;

ConstDeclList -> Result<Vec<ASTConstDeclT>, ()>:
    OneConstDecl ';' { $1 }
    | ConstDeclList OneConstDecl ';' { Ok([$1?, $2?].concat()) }
;

OneConstDecl -> Result<Vec<ASTConstDeclT>, ()>:
    Lv6Id ':' Type {
        Ok(vec![ASTConstDeclT {
            name: $1?,
            ttype: Some($3?),
            val: None
        }])
    }
    | Lv6Id ',' Lv6IdList ':' Type { 
        let mut res = vec![
            ASTConstDeclT {
                        name: $1?,
                        ttype: Some($5.clone()?),
                        val: None
            }];
        for v in $3? {
            res.push(
                ASTConstDeclT {
                        name: v,
                        ttype: Some($5.clone()?),
                        val: None
                    }
            )
        }
        Ok(res)
    }
    | Lv6Id ':' Type '=' Expression {
        Ok(vec![ASTConstDeclT {
            name: $1?,
            ttype: Some($3?),
            val: Some($5?)
        }])
    }
    | Lv6Id '=' Expression {
        Ok(vec![
            ASTConstDeclT {
                name: $1?,
                ttype: None,
                val: Some($3?)
            }
        ])
    }
;

Lv6IdList -> Result<Vec<Span>, ()>:
    Lv6Id {
        Ok(vec![$1?])
    }
    | Lv6IdList ',' Lv6Id {
        flatten($1, $3)
    }
;

Lv6Id -> Result<Span, ()>:
    'label' {
        Ok($1.map_err(|_| ())?.span())
    }
;

Type -> Result<ASTTypeT, ()>:
    'BOOL' {
        Ok(ASTTypeT::ASTBool)
    }
    | 'INT' {
        Ok(ASTTypeT::ASTInt)
    }
    | 'REAL' {
        Ok(ASTTypeT::ASTReal)
    }
    | Lv6Id {
        Ok(ASTTypeT::ASTLabel($1?))
    }
    | Type '^' Expression {
        Ok(ASTTypeT::ASTVec(Box::new($3?), Box::new($1?)))
    }
;

Expression -> Result<ASTExprT, ()>:
    Constant {
        Ok(ASTExprT::ASTConst($span, ASTTypeT::ASTNone, $1?))
    }
    | Lv6Id {
        Ok(ASTExprT::ASTVar($1?, ASTTypeT::ASTNone))
    }
    | 'NOT' Expression {
        Ok(ASTExprT::ASTUnop($span, ASTTypeT::ASTNone, ASTNot, Box::new($2?)))
    }
    | '-' Expression {
        Ok(ASTExprT::ASTUnop($span, ASTTypeT::ASTNone, ASTNot, Box::new($2?)))
    }
    | 'PRE' Expression {
        Ok(ASTExprT::ASTUnop($span, ASTTypeT::ASTNone, ASTPre, Box::new($2?)))
    }
    | 'CURRENT' Expression {
        Ok(ASTExprT::ASTUnop($span, ASTTypeT::ASTNone, ASTCurrent, Box::new($2?)))
    }
    | 'INT' Expression {
        Ok(ASTExprT::ASTUnop($span, ASTTypeT::ASTNone, ASTInt, Box::new($2?)))
    }
    | 'REAL' Expression {
        Ok(ASTExprT::ASTUnop($span, ASTTypeT::ASTNone, ASTReal, Box::new($2?)))
    }
    | Expression 'WHEN' ClockExpression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTWhen, Box::new($1?), Box::new($3?)))
    }
    | Expression 'FBY' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTFby, Box::new($1?), Box::new($3?)))
    }
    | Expression '->' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTArrow, Box::new($1?), Box::new($3?)))
    }
    | Expression 'AND' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTAnd, Box::new($1?), Box::new($3?)))
    }
    | Expression 'OR' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTOr, Box::new($1?), Box::new($3?)))
    }
    | Expression 'XOR' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTXor, Box::new($1?), Box::new($3?)))
    }
    | Expression '=>' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTImpl, Box::new($1?), Box::new($3?)))
    }
    | Expression '=' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTEq, Box::new($1?), Box::new($3?)))
    }
    | Expression '<>' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTNeq, Box::new($1?), Box::new($3?)))
    }
    | Expression '<' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTLt, Box::new($1?), Box::new($3?)))
    }
    | Expression '<=' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTLe, Box::new($1?), Box::new($3?)))
    }
    | Expression '>' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTGt, Box::new($1?), Box::new($3?)))
    }
    | Expression '>=' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTGe, Box::new($1?), Box::new($3?)))
    }
    | Expression 'DIV' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTDiv, Box::new($1?), Box::new($3?)))
    }
    | Expression 'MOD' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTMod, Box::new($1?), Box::new($3?)))
    }
    | Expression '-' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTSub, Box::new($1?), Box::new($3?)))
    }
    | Expression '+' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTAdd, Box::new($1?), Box::new($3?)))
    }
    | Expression '/' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTDiv, Box::new($1?), Box::new($3?)))
    }
    | Expression '*' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTMul, Box::new($1?), Box::new($3?)))
    }
    | 'IF' Expression 'THEN' Expression 'ELSE' Expression {
        Ok(ASTExprT::ASTIfThenElse($span, ASTTypeT::ASTNone, Box::new($2?), Box::new($4?), Box::new($6?)))
    }
    | CallExpression {
        Ok(ASTExprT::ASTFuncCall($span, ASTTypeT::ASTNone, ))
    }
    | '[' ExpressionList ']' {
        Ok(ASTExprT::ASTVec($span, ASTTypeT::ASTNone, Box::new($2?)))
    }
    | Expression '^' Expression {
        Ok(ASTExprT::ASTPow($span, ASTTypeT::ASTNone, Box::new($1?), Box::new($3?)))
    }
    | Expression '|' Expression {
        Ok(ASTExprT::ASTBinop($span, ASTTypeT::ASTNone, ASTConcat, Box::new($1?), Box::new($3?)))
    }
    | Expression '[' Expression ']' {
        Ok(ASTExprT::ASTGetElement($span, ASTTypeT::ASTNone, Box::new($1?), Box::new($3?)))
    }
    | Expression '[' Select ']' {
        Ok(ASTExprT::ASTGetSlice($span, ASTTypeT::ASTNone, Box::new($1?), Box::new($3?)))
    }
    | 'MERGE' Lv6Id '(' 'TRUE' '->' Expression ')' '(' 'FALSE' '->' Expression ')' {
        Ok(ASTExprT::ASTMerge($span, ASTTypeT::ASTNone, Box::new(ASTExprT::ASTVar($2?, ASTTypeT::ASTNone)), Box::new($6?), Box::new($11?)))
    }
    | '(' Expression ')' {
        $2
    }
;

Constant -> Result<ASTConstT, ()>:
    'TRUE' {
        Ok(ASTConstT::ASTBool(true))
    }
    | 'FALSE' {
        Ok(ASTConstT::ASTBool(false))
    }
    | Integer {
        Ok(ASTConstT::ASTInt($1?))
    }
    | Real {
        Ok(ASTConstT::ASTReal($1?))
    }
;

Integer -> Result<i64, ()>:
    'num' {
        $lexer.span_str($1.map_err(|_| ())?.span()).parse::<i64>().map_err(|_| ())
    }
;

Real -> Result<f64, ()>:
    'num' '.' 'num' {
        let a = $lexer.span_str($1.map_err(|_| ())?.span());
        let b = $lexer.span_str($3.map_err(|_| ())?.span());
        let c = [a, ".", b].concat();
        c.parse::<f64>().map_err(|_| ())
    }
    | 'num' '.' {
        let a = $lexer.span_str($1.map_err(|_| ())?.span());
        let c = [a, "."].concat();
        c.parse::<f64>().map_err(|_| ())
    }
;

ClockExpression -> Result<ASTExprT, ()>:
    Lv6Id {
        Ok(ASTExprT::ASTVar($1?, ASTTypeT::ASTNone))
        // Ok(ASTClockExprT::ASTPosClock($1?))
    }
    | 'NOT' Lv6Id {
        Ok(ASTExprT::ASTUnop($span, ASTTypeT::ASTNone, ASTNot, Box::new(ASTExprT::ASTVar($2?, ASTTypeT::ASTNone))))
        // Ok(ASTClockExprT::ASTNegClock($2?))
    }
;

ExpressionList -> Result<Vec<ASTExprT>, ()>:
    Expression {
        Ok(vec![$1?])
    }
    | ExpressionList ',' Expression {
        flatten($1, $3)
    }
;

CallExpression -> Result<(), ()>:
    Lv6Id '(' ExpressionList ')' {
        Ok(())
    }
;

Select -> Result<ASTSelectT, ()>:
    'num' '.' '.' Expression {
        let v = $lexer.span_str($1.map_err(|_| ())?.span()).parse::<i64>().map_err(|_| ());
        Ok(ASTSelectT {
            start: ASTExprT::ASTConst($1.map_err(|_| ())?.span(), ASTTypeT::ASTNone, ASTConstT::ASTInt(v?)),
            end: $4?,
            step: ASTExprT::ASTConst($span, ASTTypeT::ASTNone, ASTConstT::ASTInt(1)),
        })
    }
    | Lv6Id '.' '.' Expression {
        Ok(ASTSelectT {
            start: ASTExprT::ASTVar($1?, ASTTypeT::ASTNone),
            end: $4?,
            step: ASTExprT::ASTConst($span, ASTTypeT::ASTNone, ASTConstT::ASTInt(1)),
        })
    }
    | '(' Expression ')' '.' '.' Expression {
        Ok(ASTSelectT {
            start: $2?,
            end: $6?,
            step: ASTExprT::ASTConst($span, ASTTypeT::ASTNone, ASTConstT::ASTInt(1)),
        })
    }
    | 'num' '.' '.' Expression 'STEP' Expression {
        let v = $lexer.span_str($1.map_err(|_| ())?.span()).parse::<i64>().map_err(|_| ());
        Ok(ASTSelectT {
            start: ASTExprT::ASTConst($1.map_err(|_| ())?.span(), ASTTypeT::ASTNone, ASTConstT::ASTInt(v?)),
            end: $4?,
            step: $6?
        })
    }
    | Lv6Id '.' '.' Expression 'STEP' Expression {
        Ok(ASTSelectT {
            start: ASTExprT::ASTVar($1?, ASTTypeT::ASTNone),
            end: $4?,
            step: $6?
        })
    }
    | '(' Expression ')' '.' '.' Expression 'STEP' Expression {
        Ok(ASTSelectT {
            start: $2?,
            end: $6?,
            step: $8?
        })
    }
;
%%

use lrpar::Span;

use super::*;
use ASTBinopT::*;
use ASTUnopT::*;

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>)
           -> Result<Vec<T>, ()> {
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}