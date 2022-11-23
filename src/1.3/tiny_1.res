module TinyLang = {
    type rec expr = 
        | Cst(int)
        | Add(expr, expr)
        | Mul(expr, expr)
        | Var(string)
        | Let(string, expr, expr)

    type env = list<(string, int)>

    let rec eval = (expr: expr, env: env) => {
        // Big step operational semantics
        switch expr {
        | Cst(i) => i
        | Add(a, b) => eval(a, env) + eval(b, env)
        | Mul(a, b) => eval(a, env) * eval(b, env)
        | Var(x) => List.assoc(x, env)
        | Let (x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
        }
    }
}

module NameLess = {
    type rec expr =
        | Cst(int)
        | Add(expr, expr)
        | Mul(expr, expr)
        | Var(int)
        | Let(expr, expr)

    type env = list<int>

    let rec eval = (expr: expr, env: env) => {
        switch expr {
        | Cst(i) => i
        | Add(a, b) => eval(a, env) + eval(b, env)
        | Mul(a, b) => eval(a, env) * eval(b, env)
        | Var(n) => List.nth(env, n)
        | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
        }
    }
}

module StackWithEnv = {
    type operand = int
    type instr = Cst(operand) | Add | Mul | Var(string) | Let(string)
    type instrs = list<instr>
    type stack = list<operand>
    type env = list<(string, operand)>

    let rec eval = (instrs: instrs, stk: stack, env: env) => {
        switch (instrs, stk) {
        | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk}, env)
        | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a+b, ...stk}, env)
        | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a*b, ...stk}, env)
        | (list{Var(i), ...rest}, _) => eval(rest, list{List.assoc(i, env), ...stk}, env)
        | (list{Let(x), ...rest}, list{e, ...stk}) => eval(rest, stk, list{(x, e), ...env})
        | (list{}, list{i, ..._}) => i
        | _ => assert false
        }
    }
}

module StackMachine = {
    type operand = int
    type instr = Cst(operand) | Add | Mul | Var(operand) | Pop | Swap
    type instrs = list<instr>
    type stack = list<operand>

    let rec eval = (instrs: instrs, stk: stack) => {
        // Small step opertional semantics
        switch (instrs, stk) {
        | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk})
        | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a+b, ...stk})
        | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a*b, ...stk})
        | (list{Var(i), ...rest}, _) => eval(rest, list{List.nth(stk, i), ...stk})
        | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
        | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
        | (list{}, list{i, ..._}) => i
        | _ => assert false
        }
    }
}

let rec index = (l: list<'a>, x: 'a) => {
    switch l {
    | list{hd, ...rest} => {
        if (hd == x) { 0 } else { 1 + index(rest, x) }
      }
    | list{} => 0
    }
}

module Compiler1 = {
    // TinyLang -> NameLess -> StackMachine
    type cenv = list<string>
    let rec eraseName = (expr: TinyLang.expr, cenv: cenv): NameLess.expr => {
        switch expr {
        | Cst(i) => Cst(i)
        | Add(a, b) => Add(eraseName(a, cenv), eraseName(b, cenv))
        | Mul(a, b) => Mul(eraseName(a, cenv), eraseName(b, cenv))
        | Var(x) => Var(index(cenv, x))
        | Let(x, e1, e2) => Let(eraseName(e1, cenv), eraseName(e2, list{x, ...cenv}))
        }
    }

    let rec nl2sm = (expr: NameLess.expr): StackMachine.instrs => {
        // NameLess to StackMachine
        switch expr {
        | Cst(i) => list{StackMachine.Cst(i)}
        | Add(a, b) => Belt.List.concatMany([nl2sm(a), nl2sm(b), list{StackMachine.Add}])
        | Mul(a, b) => Belt.List.concatMany([nl2sm(a), nl2sm(b), list{StackMachine.Mul}])
        | Var(n) => list{Var(n)}
        | Let(e1, e2) => Belt.List.concatMany([nl2sm(e1), nl2sm(e2), list{Swap, Pop}])
        }
    }

    let compile = (expr: TinyLang.expr): StackMachine.instrs => {
        let nl_expr = eraseName(expr, list{})
        nl2sm(nl_expr)
    }
}


module Compiler2 = {
    // TinyLang -> StackWithEnv -> StackMachine
    let rec expr2stenv = (expr: TinyLang.expr): StackWithEnv.instrs => {
        switch expr {
        | Cst(i) => list{Cst(i)}
        | Add(a, b) => Belt.List.concatMany([expr2stenv(a), expr2stenv(b), list{Add}])
        | Mul(a, b) => Belt.List.concatMany([expr2stenv(a), expr2stenv(b), list{Mul}])
        | Var(x) => list{Var(x)}
        | Let(x, e1, e2) => Belt.List.concatMany([expr2stenv(e1), list{Let(x)}, expr2stenv(e2)])
        }
    }

    type cenv = list<string>
    
    let rec stenv2stm = (instrs: StackWithEnv.instrs, cenv: cenv): StackMachine.instrs => {
        switch instrs {
        | list{Cst(i), ...rest} => list{Cst(i), ...stenv2stm(rest, cenv)}
        | list{Add, ...rest} => list{Add, ...stenv2stm(rest, cenv)}
        | list{Mul, ...rest} => list{Mul, ...stenv2stm(rest, cenv)}
        | list{Let(x), ...rest} => {
            let code1 = stenv2stm(rest, list{x, ...cenv})
            let code2 = list{StackMachine.Swap, StackMachine.Pop}
            Belt.List.concat(code1, code2)
          }
        | list{Var(x), ...rest} => list{Var(index(cenv, x)), ...stenv2stm(rest, cenv)}
        | list{} => list{}
        }
    }

    let compile = (expr: TinyLang.expr): StackMachine.instrs => {
        let stenv_code = expr2stenv(expr)
        stenv2stm(stenv_code, list{})
    }
}

module Test = {
    let expr1: TinyLang.expr = Add(Cst(1), Mul(Cst(2), Cst(3)))
    assert (TinyLang.eval(expr1, list{}) == 7)
    let expr2: TinyLang.expr = Let("x", Add(Cst(1), Cst(2)), Var("x"))
    assert (TinyLang.eval(expr2, list{}) == 3)
    let expr3: TinyLang.expr = Var("y")
    assert (TinyLang.eval(expr3, list{("y", 3)}) == 3)

    assert (index(list{1, 2, 0, 3}, 0) == 2)
    assert (index(list{1}, 1) == 0)
    assert (index(list{1, 2, 0, 3}, 4) == 4)

    let nl2 = Compiler1.eraseName(expr2, list{})
    assert (NameLess.eval(nl2, list{}) == TinyLang.eval(expr2, list{}))

    let stenv_code1: StackWithEnv.instrs = list{Cst(10), Let("x"), Cst(1), Cst(2), Var("x"), Add, Mul}
    assert (StackWithEnv.eval(stenv_code1, list{}, list{}) == 12)
    let stenv_expr2_code = Compiler2.expr2stenv(expr2)
    assert (StackWithEnv.eval(stenv_expr2_code, list{}, list{}) == TinyLang.eval(expr2, list{}))
    
    let st_code_1: StackMachine.instrs = list{Cst(40), Cst(2), Add, Cst(10), Mul}
    assert (StackMachine.eval(st_code_1, list{}) == 420)
    let st_code_2: StackMachine.instrs = list{Cst(1), Cst(2), Var(0), Var(2), Add, Mul, Add}
    assert (StackMachine.eval(st_code_2, list{}) == 7)
    let st_code_3: StackMachine.instrs = list{Cst(1), Cst(2), Swap, Pop}
    assert (StackMachine.eval(st_code_3, list{}) == 2)

    let st_code_expr1 = Compiler1.compile(expr1)
    assert (TinyLang.eval(expr1, list{}) == (StackMachine.eval(st_code_expr1, list{})))
    let st_code_expr2 = Compiler1.compile(expr2)
    assert (TinyLang.eval(expr2, list{}) == (StackMachine.eval(st_code_expr2, list{})))
    
    let st_code2_expr2 = Compiler2.compile(expr2)
    assert (TinyLang.eval(expr2, list{}) == (StackMachine.eval(st_code2_expr2, list{})))
    let expr4: TinyLang.expr = Let("x", Mul(Add(Cst(1), Cst(2)), Cst(3)), Add(Var("x"), Var("x")))
    let st_code2_expr4 = Compiler2.compile(expr4)
    assert (TinyLang.eval(expr4, list{}) == (StackMachine.eval(st_code2_expr4, list{})))
}
