namespace SharpLogic

module Formula =

    type Formula =
        Var of string
        | Const of int
        | Disj of Formula * Formula
        | Conj of Formula * Formula
        | Neg of Formula
        | Bic of Formula * Formula
        | Impl of Formula * Formula
       
    let private simplify1 formula =
        match formula with
        Disj(Const(m), Const(n)) -> Const(m + n)
        | Conj(Const(0), x) -> Const(0)
        | Conj(x, Const(0)) -> Const(0)
        | Conj(Const(m), Const(n)) -> Const(m * n)
        | Conj(Const(1), x) -> x
        | Conj(x, Const(1)) -> x
        | Disj(Const(0), x) -> x
        | Disj(x, Const(0)) -> x
        | _ -> formula;

    let rec Simplify frm =
        match frm with
            Disj(e1, e2) -> simplify1(Disj(Simplify e1, Simplify e2))
            | Conj(e1,e2) -> simplify1(Conj(Simplify e1, Simplify e2))
            | _ -> simplify1 frm;

    let IsFormulaValid formula =
        match formula with
        Var(n) -> true
        | Neg(n) -> true
        | Conj(Const(n), m) -> true
        | Conj(n, Const(m)) -> true
        | Conj(n, m) -> true
        | Disj(Const(n), m) -> true
        | Disj(n, Const(m)) -> true
        | Disj(n, m) -> true
        | Bic(n, m) -> true
        | Impl(n, m) -> true
        | _ -> false
