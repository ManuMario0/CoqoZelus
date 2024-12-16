use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;

fn main() {
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir("parser/lustre.y")
                .unwrap()
        })
        .lexer_in_src_dir("parser/lustre.l")
        .unwrap()
        .build()
        .unwrap();
}