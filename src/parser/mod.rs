use std::fs;
use std::sync::{LazyLock, OnceLock};

use lrlex::{lrlex_mod, DefaultLexerTypes, LRNonStreamingLexerDef};
use lrpar::{lrpar_mod, NonStreamingLexer};

lrlex_mod!("parser/lustre.l");
lrpar_mod!("parser/lustre.y");

use lrpar::Span;

pub type ASTProgramT = Vec<ASTOneDeclT>;

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
pub enum ASTUnopT {
    ASTNot,
    ASTMinus,
    ASTPre,
    ASTCurrent,
    ASTInt,
    ASTReal,
}

#[derive(Debug, Clone)]
pub enum ASTBinopT {
    ASTWhen,
    ASTFby,
    ASTArrow,
    ASTAnd,
    ASTOr,
    ASTXor,
    ASTImpl,
    ASTEq,
    ASTNeq,
    ASTLt,
    ASTLe,
    ASTGt,
    ASTGe,
    ASTDiv,
    ASTMod,
    ASTSub,
    ASTAdd,
    ASTMul,
    ASTConcat,
}

#[derive(Debug, Clone)]
pub enum ASTNameT {
    ASTSpan(Span),
    ASTString(String),
}

impl ASTNameT {
    pub fn get_name<'a>(self, obj: &FileObj<'a, 'a>) -> String {
        match self {
            ASTNameT::ASTSpan(span) => obj.span_str(span),
            ASTNameT::ASTString(s) => s,
        }
    }

    pub fn get_span(self) -> Span {
        match self {
            ASTNameT::ASTSpan(span) => span,
            ASTNameT::ASTString(_) => Span::new(0, 0),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ASTExprT {
    ASTConst(Span, ASTTypeT, ASTConstT),
    ASTVar(ASTNameT, ASTTypeT),
    ASTUnop(Span, ASTTypeT, ASTUnopT, Box<ASTExprT>),
    ASTBinop(Span, ASTTypeT, ASTBinopT, Box<ASTExprT>, Box<ASTExprT>),
    ASTIfThenElse(Span, ASTTypeT, Box<ASTExprT>, Box<ASTExprT>, Box<ASTExprT>),
    ASTFuncCall(Span, ASTTypeT),
    ASTVec(Span, ASTTypeT, Box<Vec<ASTExprT>>),
    ASTPow(Span, ASTTypeT, Box<ASTExprT>, Box<ASTExprT>),
    ASTGetElement(Span, ASTTypeT, Box<ASTExprT>, Box<ASTExprT>),
    ASTGetSlice(Span, ASTTypeT, Box<ASTExprT>, Box<ASTSelectT>),
    ASTList(Span, ASTTypeT, Box<Vec<ASTExprT>>),
    ASTMerge(Span, ASTTypeT, Box<ASTExprT>, Box<ASTExprT>, Box<ASTExprT>),
}

#[derive(Debug, Clone)]
pub struct ASTSelectT {
    pub start: ASTExprT,
    pub end: ASTExprT,
    pub step: ASTExprT,
}

#[derive(Debug, Clone)]
pub enum ASTTypeT {
    ASTNone,
    ASTBool,
    ASTInt,
    ASTReal,
    ASTLabel(Span),
    ASTVec(Box<ASTExprT>, Box<ASTTypeT>),
}

impl PartialEq for ASTTypeT {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ASTTypeT::ASTBool, ASTTypeT::ASTBool) => true,
            (ASTTypeT::ASTInt, ASTTypeT::ASTInt) => true,
            (ASTTypeT::ASTReal, ASTTypeT::ASTReal) => true,
            _ => false,
        }
    }
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
    pub name: ASTNameT,
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
    ASTVar(ASTNameT),
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

    pub fn print_error_message(&self, span: lrpar::Span, msg: &str) {
        let ((line, col), _) = self.line_col(span);
        println!("[ ERROR ] {msg} : at line {line}, col {col}");
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
