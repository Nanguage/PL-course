open Belt.List

module TinyLang = {
    type rec expr =
        | Cst(int)
        | Add(expr, expr)
        | Mul(expr, expr)
        | Var(string)
        | Let(string, expr, expr)
        | Fn(list<string>, expr)
        | App(expr, list<expr>)

    type rec value =
        | Vint(int)
        | VClosure(env, list<string>, expr)
    and env = list<(string, value)>

    let vadd = (v1, v2): value => {
        switch (v1, v2) {
        | (Vint(i1), Vint(i2)) => Vint(i1 + i2)
        | _ => {assert false}
        }
    }

    let vmul = (v1, v2): value => {
        switch (v1, v2) {
        | (Vint(i1), Vint(i2)) => Vint(i1 * i2)
        | _ => {assert false}
        }
    }

    let rec eval = (expr: expr, env: env): value => {
        switch expr {
        | Cst(i) => Vint(i)
        | Add(a, b) => vadd(eval(a, env), eval(b, env))
        | Mul(a, b) => vmul(eval(a, env), eval(b, env))
        | Var(x) => List.assoc(x, env)
        | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
        | Fn(xs, e) => VClosure(env, xs, e)
        | App(e, es) => {
            switch eval(e, env) {
            | VClosure(env_closure, xs, body) => {
                let vs = map(es, e => eval(e, env))
                let fun_env = concatMany([zip(xs, vs), env_closure])
                eval(body, fun_env)
            }
            | _ => {assert false}
            }
        }
        }
    }
}


module Test = {
    let expr1: TinyLang.expr = Let("add1", Fn(list{"x"}, Add(Var("x"), Cst(1))), App(Var("add1"), list{Cst(41)}))
    assert (TinyLang.eval(expr1, list{}) == TinyLang.Vint(42))
}
