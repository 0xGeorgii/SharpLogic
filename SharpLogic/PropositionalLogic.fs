namespace SharpLogic

module PropositionalLogic =

    open SharpLogic.BasicAlgorithms
    open Microsoft.FSharp.Core.Printf
    open SharpLogic.Formula



    //TODO: write unit tests
    let rec BuildFormulaCalcList formula =
        match formula with
        | Var (n) -> [ Var(n) ]
        | Disj (n, m) ->
            formula
            :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | Conj (n, m) ->
            formula
            :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | Neg (n) -> formula :: BuildFormulaCalcList(n)
        | Bic (n, m) ->
            formula
            :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | Impl (n, m) ->
            formula
            :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | _ -> [ formula ]

    //TODO: write unit tests
    //TODO: write a wiki page
    let CalcFormula formula =
        match formula with
        | Conj (Const (X), Const (Y)) -> X && Y
        | Disj (Const (X), Const (Y)) -> X || Y
        | Neg (Const (X)) -> not X
        | Bic (Const (X), Const (Y)) -> X = Y
        | Impl (Const (X), Const (Y)) ->
            match (X, Y) with
            | (true, true) -> true
            | (true, false) -> false
            | (false, true) -> true
            | (false, false) -> true
        | _ -> failwithf "Unsupported formula %A" formula //TODO: what should be here?

    //TODO: write unit tests
    let BuildAllFormulasInterpritations formulaCalcList =
        let vars =
            formulaCalcList
            |> List.filter (function
                | Formula.Var (_) -> true
                | _ -> false)

        let data = List.init vars.Length (fun _ -> [ true; false ])
        cartList data
