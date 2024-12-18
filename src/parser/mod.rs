use std::fs;
use std::sync::{LazyLock, OnceLock};

use lrlex::{lrlex_mod, DefaultLexerTypes, LRNonStreamingLexerDef};
use lrpar::{lrpar_mod, NonStreamingLexer};

lrlex_mod!("parser/lustre.l");
lrpar_mod!("parser/lustre.y");

use lrpar::Span;

pub type ASTProgramT = Vec<ASTOneDeclT>;

#[derive(Debug, Clone)]
pub enum ASTClockExprT {
    ASTPosClock(Span),
    ASTNegClock(Span),
}

#[derive(Debug, Clone)]
pub enum ASTConstT {
    ASTBool(bool),
    ASTInt(i64),
    ASTReal(f64),
}

#[derive(Debug, Clone)]
pub enum ASTOneDeclT {
    ASTConstDecl(Vec<ASTConstDeclT>),
    ASTTypeDecl(Vec<ASTTypeDeclT>),
    ASTNodeDecl(ASTNodeDeclT),
}

#[derive(Debug, Clone)]
pub enum ASTExprT {
    ASTConst(ASTConstT),
    ASTVar(Span),
    ASTNot(Box<ASTExprT>),
    ASTMinus(Box<ASTExprT>),
    ASTPre(Box<ASTExprT>),
    ASTCurrent(Box<ASTExprT>),
    ASTInt(Box<ASTExprT>),
    ASTReal(Box<ASTExprT>),
    ASTWhen(Box<ASTExprT>, ASTClockExprT),
    ASTFby(Box<ASTExprT>, Box<ASTExprT>),
    ASTArrow(Box<ASTExprT>, Box<ASTExprT>),
    ASTAnd(Box<ASTExprT>, Box<ASTExprT>),
    ASTOr(Box<ASTExprT>, Box<ASTExprT>),
    ASTXor(Box<ASTExprT>, Box<ASTExprT>),
    ASTImpl(Box<ASTExprT>, Box<ASTExprT>),
    ASTEq(Box<ASTExprT>, Box<ASTExprT>),
    ASTNeq(Box<ASTExprT>, Box<ASTExprT>),
    ASTLt(Box<ASTExprT>, Box<ASTExprT>),
    ASTLe(Box<ASTExprT>, Box<ASTExprT>),
    ASTGt(Box<ASTExprT>, Box<ASTExprT>),
    ASTGe(Box<ASTExprT>, Box<ASTExprT>),
    ASTDiv(Box<ASTExprT>, Box<ASTExprT>),
    ASTMod(Box<ASTExprT>, Box<ASTExprT>),
    ASTSub(Box<ASTExprT>, Box<ASTExprT>),
    ASTAdd(Box<ASTExprT>, Box<ASTExprT>),
    ASTMul(Box<ASTExprT>, Box<ASTExprT>),
    ASTIfThenElse(Box<ASTExprT>, Box<ASTExprT>, Box<ASTExprT>),
    ASTFuncCall(),
    ASTVec(Box<Vec<ASTExprT>>),
    ASTPow(Box<ASTExprT>, Box<ASTExprT>),
    ASTGetElement(Box<ASTExprT>, Box<ASTExprT>),
    ASTGetSlice(Box<ASTExprT>, Box<ASTSelectT>),
    ASTConcat(Box<ASTExprT>, Box<ASTExprT>),
    ASTList(Box<Vec<ASTExprT>>),
}

#[derive(Debug, Clone)]
pub struct ASTSelectT {
    pub start: ASTExprT,
    pub end: ASTExprT,
    pub step: ASTExprT,
}

#[derive(Debug, Clone)]
pub enum ASTTypeT {
    ASTBool,
    ASTInt,
    ASTReal,
    ASTVec(ASTExprT, Box<ASTTypeT>),
}

/*
    At the end, we will simply inline the constants``
    (after computation, e.i. cbv).
    This will be done in the first pass of the compilation,
    after parsing
*/

#[derive(Debug, Clone)]
pub struct ASTConstDeclT {
    pub name: Span,
    pub ttype: Option<ASTTypeT>,
    pub val: Option<ASTExprT>,
}

#[derive(Debug, Clone)]
pub struct ASTTypeDeclT {
    pub name: Span,
    pub ttype: ASTTypeT,
}

#[derive(Debug, Clone)]
pub struct ASTNodeDeclT {
    pub name: Span,
    pub params: Vec<ASTVarT>,
    pub returns: Vec<ASTVarT>,
    pub localVars: Vec<ASTVarT>,
    pub body: ASTBodyT,
}

#[derive(Debug, Clone)]
pub struct ASTVarT {
    pub name: Span,
    pub ttype: ASTTypeT,
}

pub type ASTBodyT = Vec<ASTEquationT>;

#[derive(Debug, Clone)]
pub struct ASTEquationT {
    pub lhs: ASTLeftT,
    pub rhs: ASTExprT,
}

#[derive(Debug, Clone)]
pub enum ASTLeftT {
    ASTAssert(),
    ASTLeftItem(Vec<ASTLeftItemT>),
}

#[derive(Debug, Clone)]
pub enum ASTLeftItemT {
    ASTVar(Span),
    ASTTableElement(Box<ASTLeftItemT>, ASTExprT),
    ASTTableSlice(Box<ASTLeftItemT>, ASTSelectT),
}

pub struct FileObj<'lexer, 'input> {
    ast: ASTProgramT,
    lexer: lrlex::LRNonStreamingLexer<'lexer, 'input, lrlex::DefaultLexerTypes>,
}

impl FileObj<'_, '_> {
    pub fn new(f: &'static str) -> Result<FileObj<'_, '_>, String> {
        static LEXERDEF: LazyLock<LRNonStreamingLexerDef<DefaultLexerTypes>> =
            LazyLock::new(|| lustre_l::lexerdef());
        static CONTENT: OnceLock<String> = OnceLock::new();

        CONTENT.set(fs::read_to_string(f).unwrap()).unwrap();
        let lexer = LEXERDEF.lexer(&CONTENT.get().unwrap());
        let (res, errs) = lustre_y::parse(&lexer);
        let mut s = "".to_string();
        for e in errs {
            s = [s, e.pp(&lexer, &lustre_y::token_epp), "\n".to_string()].concat();
        }
        match res {
            Some(Ok(v)) => Ok(FileObj { ast: v, lexer }),
            _ => Err(s),
        }
    }

    pub fn get_ast(&self) -> &ASTProgramT {
        &self.ast
    }

    pub fn get_mut_ast(&mut self) -> &mut ASTProgramT {
        &mut self.ast
    }

    pub fn set_ast(&mut self, ast: ASTProgramT) {
        self.ast = ast;
    }

    pub fn span_str(&self, span: lrpar::Span) -> String {
        self.lexer.span_str(span).to_string()
    }

    pub fn span_lines_str(&self, span: lrpar::Span) -> String {
        self.lexer.span_lines_str(span).to_string()
    }

    pub fn line_col(&self, span: lrpar::Span) -> ((usize, usize), (usize, usize)) {
        self.lexer.line_col(span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debugger() {
        match FileObj::new("/Users/emmanuel/Documents/developement/shared-projects/reactive-sys/CoqoZelus/tests/test") {
            Ok(obj) => {
                println!("{:?}", obj.get_ast());
                assert!(true);
            }
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        }
    }
}
