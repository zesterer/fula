use fula_syntax::{
    token::TokenList,
    ast::Ast,
};
use fula_compiler::hir;

fn main() {
    let code = r#"
        const len: List -> _ = |l|
            if len(l) = 0
            then 0
            else 1 + len(tail(l));

        const io_test: @ -> @ =
            print("Hello, world!");

        const main: @ -> Int =
            let x = 3 + 4;
            if x = 8
            then if 4 < 7
                then || 1337
                else 64
            else 42;
    "#;

    let tokens = TokenList::lex(code).unwrap();
    println!("Tokens: {:?}", tokens);

    let ast = Ast::parse(&tokens).unwrap();
    println!("AST: {:#?}", ast);

    let mut hir = hir::Program::from(&ast);
    println!("HIR: {:#?}", hir);

    let program = hir.compile();
    println!("Program: {:#?}", program);
}
