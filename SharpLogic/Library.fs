namespace SharpLogic
open SharpLogic.BasicAlgorithms
open System.Collections.Generic

module Formula =
    open Microsoft.FSharp.Core.Printf

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
        
    //TODO: write unit tests
    //TODO: write a wiki page
    let rec CalcFormulaDepth formula =
        match formula with
        | Var(n) -> 1
        | Disj(n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | Conj(n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | Neg(n) -> 1 + CalcFormulaDepth(n)
        | Bic(n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | Impl(n, m) -> 1 + CalcFormulaDepth(n) + CalcFormulaDepth(m)
        | _ -> 1

    //TODO: write unit tests
    let rec BuildFormulaCalcList formula =
        match formula with
        | Var(n) -> [Var(n)]
        | Disj(n, m) -> formula :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | Conj(n, m) -> formula :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | Neg(n) -> formula :: BuildFormulaCalcList(n)
        | Bic(n, m) -> formula :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | Impl(n, m) -> formula :: (BuildFormulaCalcList(n) @ BuildFormulaCalcList(m))
        | _ -> [ formula ]

    //TODO: write unit tests
    //TODO: write a wiki page
    let CalcFormula formula =
        match formula with
        | Conj(Const(X), Const(Y)) -> X && Y
        | Disj(Const(X), Const(Y)) -> X || Y
        | Neg(Const(X)) -> not X
        | Bic(Const(X), Const(Y)) -> X = Y
        | Impl(Const(X), Const(Y)) ->
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
                |> List.filter
                (function 
                    |Formula.Var(_) -> true
                    | _ -> false
                )
        let data = List.init vars.Length (fun _ -> [ true; false ])
        cartList data

    //TODO: write unit tests
    let VerboseTableuxCalculus (formulaCalcList: Formula list) (formulaInterpritations: bool list list) : string =
        let sb = new System.Text.StringBuilder()
        let subFormulasVals = new Dictionary<Formula, bool>()
        let _saveFormulaValueAndPrint = fun formula value ->
            if not(subFormulasVals.ContainsKey(formula)) then subFormulasVals.Add(formula, value) else subFormulasVals.Item(formula) <- value
            bprintf sb $"{value}\t"
            let len = ((VerboseFormula formula).Length / 8)
            for _ = 1 to len do
                bprintf sb "\t"
        for i = 0 to formulaInterpritations.Length - 1 do
            let rowVarValues = formulaInterpritations.Item i
            let mutable j = 0
            formulaCalcList |> List.iter
                (fun f ->
                            match f with
                            | Formula.Var(_) ->
                                bprintf sb $"{rowVarValues.Item(j)} \t"
                                j <- j + 1
                            | Formula.Neg(Var(x)) -> 
                                let index = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                                let calc = CalcFormula(Formula.Neg(Formula.Const(rowVarValues.Item(index))))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Conj(Var(x), Var(y)) ->
                                let indexX = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                                let indexY = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                                let calc = CalcFormula(Conj(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Disj(Var(x), Var(y)) ->
                                let indexX = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                                let indexY = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                                let calc = CalcFormula(Disj(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Bic(Var(x), Var(y)) ->
                                let indexX = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                                let indexY = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                                let calc = CalcFormula(Bic(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Bic(Var(x), y) ->
                                let rightVal = subFormulasVals.GetValueOrDefault(y)
                                let indexX = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                                let calc = CalcFormula(Bic(Const(rowVarValues.Item(indexX)), Const(rightVal)))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Bic(x, Var(y)) ->
                                let leftVal = subFormulasVals.GetValueOrDefault(x)
                                let indexY = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                                let calc = CalcFormula(Bic(Const(rowVarValues.Item(indexY)), Const(leftVal)))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Bic(x, y) ->
                                let leftVal = subFormulasVals.GetValueOrDefault(x)
                                let rightVal = subFormulasVals.GetValueOrDefault(y)
                                let calc = CalcFormula(Bic(Const(rightVal), Const(leftVal)))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Impl(Var(x), Var(y)) ->
                                let leftVal = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                                let rightVal = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                                let calc = CalcFormula(Impl(Const(rowVarValues.Item(leftVal)), Const(rowVarValues.Item(rightVal))))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Impl(x, Var(y)) ->
                                let leftVal = subFormulasVals.GetValueOrDefault(x)
                                let rightVal = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                                let calc = CalcFormula(Impl(Const(leftVal), Const(rowVarValues.Item(rightVal))))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Impl(Var(x), y) ->
                                let rightVal = subFormulasVals.GetValueOrDefault(y)
                                let leftVal = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                                let calc = CalcFormula(Impl(Const(rowVarValues.Item(leftVal)), Const(rightVal)))
                                _saveFormulaValueAndPrint f calc
                            | Formula.Impl(x, y) ->
                                let leftVal = subFormulasVals.GetValueOrDefault(x)
                                let rightVal = subFormulasVals.GetValueOrDefault(y)
                                let calc = CalcFormula(Impl(Const(leftVal), Const(rightVal)))
                                _saveFormulaValueAndPrint f calc
                            | _ -> printf "unknown formula %A" f
                )
            bprintf sb "\r\n"
        sb.ToString()
