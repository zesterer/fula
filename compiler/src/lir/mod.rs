use std::collections::HashMap;

pub struct Program {
    funcs: HashMap<usize, Func>,
}

pub struct Func {
    blocks: HashMap<usize, Block>,
}

pub struct Block {
    instrs: Vec<Instr>,
    branch: Branch,
}

pub enum Branch {
    Return,
    If(usize, usize),
}

pub enum Instr {
    LoadLocal(usize),
    StoreLocal(usize),
    MakeFuncEnvironment {
        func: usize,
        locals: Vec<usize>,
    },
    Call,
    CallGlobal(usize),
}
