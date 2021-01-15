namespace SharpLogic

module Formula =

    type Formula =
        Var of string
        | Const of int
        | Add of Formula * Formula
        | Mul of Formula * Formula
        | Neg of Formula
        | Eq of Formula * Formula
        | Impl of Formula * Formula
       
    let simplify1 formula =
        match formula with
        Add(Const(m), Const(n)) -> Const(m+n)
        | Mul(Const(0), x) -> Const(0)
        | Mul(x, Const(0)) -> Const(0)
        | Mul(Const(m), Const(n)) -> Const(m*n)
        | Mul(Const(1), x) -> x
        | Mul(x, Const(1)) -> x
        | Add(Const(0), x) -> x
        | Add(x, Const(0)) -> x
        | _ -> formula;

    let rec simplify frm =
        match frm with
            Add(e1, e2) -> simplify1(Add(simplify e1, simplify e2))
            | Mul(e1,e2) -> simplify1(Mul(simplify e1, simplify e2))
            | _ -> simplify1 frm;

    let isFormulaValid formula =
        match formula with
        Var(n) -> true
        | Neg(n) -> true
        | Mul(Const(n), m) -> true
        | Mul(n, Const(m)) -> true
        | Mul(n, m) -> true
        | Add(n, m) -> true
        | Eq(n, m) -> true
        | Impl(n, m) -> true
        | _ -> false
            
