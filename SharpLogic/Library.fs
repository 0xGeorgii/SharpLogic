namespace SharpLogic

module Expressions =

    type Expression =
        Var of string
        | Const of int
        | Add of Expression * Expression
        | Mul of Expression * Expression
       
    let simplify1 expression =
        match expression with
        Add(Const(m), Const(n)) -> Const(m+n)
        | Mul(Const(m), Const(n)) -> Const(m+n)
        | Add(Const(0), x) -> x
        | Add(x, Const(0)) -> x
        | Mul(Const(0), x) -> Const(0)
        | Mul(x, Const(0)) -> Const(0)
        | Mul(Const(1), x) -> x
        | Mul(x, Const(1)) -> x
        | _ -> expression;
