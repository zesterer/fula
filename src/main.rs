use fula_syntax::{
    SyntaxError,
    lex,
};
use fula_compiler::{
    CompileError,
    hir,
};

pub enum Error<'a, 'b> {
    Syntax(SyntaxError<'a, 'b>),
    Compile(CompileError<'a>),
}

fn display_errors(code: &str, errors: Vec<Error>) {
    for error in errors {
        match error {
            Error::Syntax(err) => println!("Syntax Error: {:?}", err),
            Error::Compile(err) => println!("Compile Error: {:?}", err),
        }
    }
}

fn main() {
    let code = r#"
        const len: List -> _ = |l|
            if len(l) = 0
            then 0
            else 1 + len(tail(l));

        const io_test: @ -> @ =
            print("Hello, world!");

        const main: @ -> @ =
            let x = 3 + 4;
            if x = 8
            then if 4 < 7
                then || 1337
                else 64
            else 42;
    "#;

    let code = r#"
        const factorial: Int -> Int = |x|
            if x = 0
            then 1
            else x * factorial(x - 1);

        const add: Int -> Int -> Int = |x, y|
            x + y;

        const fact_10: _ = factorial(10);

        const main: _ = add(3, 4);
    "#;

    let tokens = match lex(code) {
        Ok(tokens) => tokens,
        Err(errors) => {
            display_errors(code, errors
                .into_iter()
                .map(|err| Error::Syntax(err))
                .collect());
            return;
        },
    };

    let ast = match tokens.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            display_errors(code, errors
                .into_iter()
                .map(|err| Error::Syntax(err))
                .collect());
            return;
        },
    };

    let mut hir = hir::Program::from(&ast);
    let program = hir.compile();

    println!("HIR: {:#?}", hir);
    println!("Program: {:#?}", program);
}
