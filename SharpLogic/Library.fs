namespace SharpLogic

module Formula =

    type Formula =
        Var of string
        | Const of bool
        | Disj of Formula * Formula
        | Conj of Formula * Formula
        | Neg of Formula
        | Bic of Formula * Formula
        | Impl of Formula * Formula
     
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
        
    //TODO: write unit tests
    let rec VerboseFormula formula =
        match formula with
        | Var(n) -> n
        | Const(n) -> n.ToString()
        | Disj(n, m) -> $"({VerboseFormula(n)} || {VerboseFormula(m)})"
        | Conj(n, m) -> $"({VerboseFormula(n)} && {VerboseFormula(m)})"
        | Neg(n) -> $"~{VerboseFormula(n)}"
        | Bic(n, m) -> $"({VerboseFormula(n)} <=> {VerboseFormula(m)})"
        | Impl(n, m) -> $"({VerboseFormula(n)} -> {VerboseFormula(m)})"
        | _ -> ""
        
    //TODO: write unit tests
    let rec FormulaCaclDepth formula =
        match formula with
        | Var(n) -> 1
        | Disj(n, m) -> 1 + FormulaCaclDepth(n) + FormulaCaclDepth(m)
        | Conj(n, m) -> 1 + FormulaCaclDepth(n) + FormulaCaclDepth(m)
        | Neg(n) -> 1 + FormulaCaclDepth(n)
        | Bic(n, m) -> 1 + FormulaCaclDepth(n) + FormulaCaclDepth(m)
        | Impl(n, m) -> 1 + FormulaCaclDepth(n) + FormulaCaclDepth(m)
        | _ -> 1

    //TODO: write unit tests
    let rec BuildTruthTableHeaders formula =
        match formula with
        | Var(n) -> [Var(n)]
        | Disj(n, m) -> [formula] @ BuildTruthTableHeaders(n) @ BuildTruthTableHeaders(m)
        | Conj(n, m) -> [formula] @ BuildTruthTableHeaders(n) @ BuildTruthTableHeaders(m)
        | Neg(n) -> [formula] @ BuildTruthTableHeaders(n)
        | Bic(n, m) -> [formula] @ BuildTruthTableHeaders(n) @ BuildTruthTableHeaders(m)
        | Impl(n, m) -> [formula] @ BuildTruthTableHeaders(n) @ BuildTruthTableHeaders(m)
        | _ -> [ formula ]


    let CalcFormula formula =
        match formula with
        | Conj(Const(X), Const(Y)) -> X && Y
        | Disj(Const(X), Const(Y)) -> X || Y
        | Neg(Const(X)) -> not X
        | Bic(Const(X), Const(Y)) -> (X && Y) || (not X && not Y)
        | Impl(Const(X), Const(Y)) ->
            match (X, Y) with
            | (true, true) -> true
            | (true, false) -> false
            | (false, true) -> true
            | (false, false) -> true
        | _ -> false //TODO: what should be here?
