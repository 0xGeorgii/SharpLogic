open System
open SharpLogic.Formula
open SharpLogic.BasicAlgorithms
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let frm = Formula.Impl(Conj(Var "P", Var "Q"), Bic(Var "R", Neg(Var "S")))
    printf "%s\n" (VerboseFormula frm)
    let formulaCalcList = BuildFormulaCalcList frm |> List.sortBy(fun f -> FormulaCaclDepth f)
    List.iter(fun f -> printf "%A\r\n" f) formulaCalcList
    let isFormulaValid = IsFormulaValid frm
    printf $"Formula is valid: [{isFormulaValid}]\n"
    printf "%s\r\n" "================" |> ignore 
    formulaCalcList |> List.map VerboseFormula |> List.iter(fun f -> printf "%s\t" f)

    //TODO: extract as a separate function BuildAllFormulasInterpritations
    let vars = 
        formulaCalcList
            |> List.filter
            (function 
                |Formula.Var(_) -> true
                | _ -> false
            )
    let data = List.init vars.Length (fun _ -> [ true; false ])
    let res = cartList data
    //*****************

    printf ""

    //TODO: extract as a separate function VerboseTableuxCalculus
    let subFormulasVals = new Dictionary<Formula, bool>()
    let _saveFormulaValueAndPrint = fun formula value ->
        if not(subFormulasVals.ContainsKey(formula)) then subFormulasVals.Add(formula, value) else subFormulasVals.Item(formula) <- value
        printf "%b\t" value
        let len = ((VerboseFormula formula).Length / 8)
        for _ = 1 to len do
           printf "\t"
    for i = 0 to res.Length - 1 do
        let rowVarValues = res.Item i
        let mutable j = 0
        formulaCalcList |> List.iter
            (fun f ->
                        match f with
                        | Formula.Var(_) ->
                            Console.Write(rowVarValues.Item(j).ToString() + "\t")
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
                            let indexX = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                            let indexY = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                            let calc = CalcFormula(Impl(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))
                            _saveFormulaValueAndPrint f calc
                        | Formula.Impl(x, Var(y)) ->
                            let leftVal = subFormulasVals.GetValueOrDefault(x)
                            let indexY = formulaCalcList |> List.findIndex(fun h -> h = Var(y))
                            let calc = CalcFormula(Impl(Const(rowVarValues.Item(indexY)), Const(leftVal)))
                            _saveFormulaValueAndPrint f calc
                        | Formula.Impl(Var(x), y) ->
                            let rightVal = subFormulasVals.GetValueOrDefault(y)
                            let indexX = formulaCalcList |> List.findIndex(fun h -> h = Var(x))
                            let calc = CalcFormula(Impl(Const(rowVarValues.Item(indexX)), Const(rightVal)))
                            _saveFormulaValueAndPrint f calc
                        | Formula.Impl(x, y) ->
                            let leftVal = subFormulasVals.GetValueOrDefault(x)
                            let rightVal = subFormulasVals.GetValueOrDefault(y)
                            let calc = CalcFormula(Impl(Const(rightVal), Const(leftVal)))
                            _saveFormulaValueAndPrint f calc
                        | _ -> printf ""
            )
        printf "\r\n"
    //**************

    0 // return an integer exit code
