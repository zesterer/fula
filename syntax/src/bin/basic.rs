use fula_syntax::{
    token::TokenList,
    ast::Ast,
};

fn main() {
    let code = r#"
        if 3 + 5 = 8
        then if 4 < 7
            then 1337
            else 64
        else 42
    "#;

    let tokens = TokenList::lex(code).unwrap();
    println!("Tokens: {:?}", tokens);

    let ast = Ast::parse(&tokens).unwrap();
    println!("Ast: {:#?}", ast);
}
