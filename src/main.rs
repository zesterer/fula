use std::fmt;
use fula_syntax::{
    src::SrcRef,
    lex,
    SyntaxError,
};
use fula_compiler::{
    CompileError,
    hir,
};

pub enum Error<'a, 'b> {
    Syntax(SyntaxError<'a, 'b>),
    Compile(CompileError<'a>),
}

impl<'a, 'b> Error<'a, 'b> {
    pub fn get_text(&self) -> String {
        match self {
            Error::Syntax(syntax_err) => syntax_err.get_text(),
            Error::Compile(compile_err) => compile_err.get_text(),
        }
    }

    pub fn get_src_refs(&self) -> Vec<SrcRef> {
        match self {
            Error::Syntax(syntax_err) => syntax_err.get_src_refs(),
            Error::Compile(compile_err) => compile_err.get_src_refs(),
        }
    }
}

struct Repeat(char, usize);

impl fmt::Display for Repeat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..self.1 {
            write!(f, "{}", self.0)?;
        }
        Ok(())
    }
}

fn display_errors(code: &str, errors: Vec<Error>) {
    for error in errors {
        let src_refs = error.get_src_refs();

        println!("Error: {}", error.get_text());

        for ((line0, col0), (line1, col1)) in src_refs
            .into_iter()
            .filter_map(|src_ref| src_ref.in_context(code))
        {
            for line in line0..=line1 {
                let line_str = code.lines().nth(line).unwrap_or("");
                println!("{:>5} | {}", line + 1, line_str.replace('\t', "    "));
                print!("      | ");
                for (col, c) in line_str.chars().enumerate() {
                    let underline_c = if (line != line0 || col >= col0) && (line != line1 || col < col1) {
                        '^'
                    } else {
                        ' '
                    };
                    let char_w = if c == '\t' { 4 } else { 1 };
                    print!("{}", Repeat(underline_c, char_w));
                }
                print!("\n");
            }
        }
    }
}

fn main() {
    let code = include_str!("../examples/basic.fu");

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
    let program = match hir.compile() {
        Ok(program) => program,
        Err(errors) => {
            display_errors(code, errors
                .into_iter()
                .map(|err| Error::Compile(err))
                .collect());
            return;
        },
    };

    println!("HIR: {:#?}", hir);
    println!("Program: {:#?}", program);
}
