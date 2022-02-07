namespace SharpLogic

module Formula =

    type Formula =
        | Var of string
        | Const of bool
        | Disj of Formula * Formula
        | Conj of Formula * Formula
        | Neg of Formula
        | Bic of Formula * Formula
        | Impl of Formula * Formula

    let IsFormulaAcceptable formula =
        match formula with
        | Var (n) -> true
        | Neg (n) -> true
        | Conj (Const (n), m) -> true
        | Conj (n, Const (m)) -> true
        | Conj (n, m) -> true
        | Disj (Const (n), m) -> true
        | Disj (n, Const (m)) -> true
        | Disj (n, m) -> true
        | Bic (n, m) -> true
        | Impl (n, m) -> true
        | _ -> false

    //TODO: write unit tests
    //TODO: write a wiki page
    let rec CalcFormulaDepth formula =
        match formula with
        | Var (n) -> 1
        | Disj (n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | Conj (n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | Neg (n) -> 1 + CalcFormulaDepth(n)
        | Bic (n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | Impl (n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | _ -> 1
