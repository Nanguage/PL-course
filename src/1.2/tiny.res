module TinyLang = {
    type rec expr = 
        | Cst (int)
        | Add (expr, expr)
        | Mul (expr, expr)

    let rec eval = (expr: expr) => {
        // Big step operational semantics
        switch expr {
        | Cst (i) => i
        | Add(a, b) => eval(a) + eval(b)
        | Mul(a, b) => eval(a) * eval(b)
        }
    }
}

module StackMachine = {
    type instr = Cst (int) | Add | Mul
    type instrs = list <instr>
    type operand = int
    type stack = list <operand>

    let rec eval = (instrs: instrs, stk: stack) => {
        // Small step opertional semantics
        switch (instrs, stk) {
        | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk})
        | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a+b, ...stk})
        | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a*b, ...stk})
        | (list{}, list{i, _}) => i
        | (list{}, list{i}) => i
        | _ => assert false
        }
    }
}

module Compiler = {
    let rec compile = (expr: TinyLang.expr) => {
        switch expr {
        | Cst (i) => list{StackMachine.Cst(i)}
        | Add (a, b) => Belt.List.concatMany([compile(a), compile(b), list{StackMachine.Add}])
        | Mul (a, b) => Belt.List.concatMany([compile(a), compile(b), list{StackMachine.Mul}])
        }
    }
}

module Test = {
    let example_expr: TinyLang.expr = Add(Cst(1), Mul(Cst(2), Cst(3)))
    assert (TinyLang.eval(example_expr) == 7)
    let example_st_code: StackMachine.instrs = list{Cst(40), Cst(2), Add, Cst(10), Mul}
    assert (StackMachine.eval(example_st_code, list{}) == 420)
    
    let compiled = Compiler.compile(example_expr)
    assert (StackMachine.eval(compiled, list{}) == 7)
}

