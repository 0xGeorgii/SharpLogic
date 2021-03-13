open System
open SharpLogic.Formula
open SharpLogic.BasicAlgorithms
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let frm = Formula.Impl(Conj(Var "P", Var "Q"), Bic(Var "R", Neg(Var "S")))
    Console.WriteLine(VerboseFormula frm)
    let formulaCalcList = BuildFormulaCalcList frm |> List.sortBy(fun f -> FormulaCaclDepth f)
    List.iter(fun f -> printf "%A\r\n" f) formulaCalcList    
    let isFormulaValid = IsFormulaValid frm
    Console.WriteLine($"Formula is valid: [{isFormulaValid}]")
    printf "%s\r\n" "================" |> ignore 
    formulaCalcList |> List.map VerboseFormula |> List.iter(fun f -> printf "%s\t" f)
    let vars = 
        formulaCalcList
            |> List.filter
            (function 
                |Formula.Var(_) -> true
                | _ -> false
            )
    let data = List.init vars.Length (fun _ -> [ true; false ])
    let res = cartList data
    Console.WriteLine("");
    let subFormulasVals = new Dictionary<Formula, bool>()
    let _saveFormulaValueAndPrint = fun formula value ->
        if not(subFormulasVals.ContainsKey(formula)) then subFormulasVals.Add(formula, value) else subFormulasVals.Item(formula) <- value
        Console.Write(value.ToString() + "\t")
    for i = 0 to res.Length - 1 do
        let rowVarValues = res.Item i
        let mutable j = 0
        formulaCalcList |> List.iter 
            (function f ->
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
                        | _ -> Console.WriteLine("")
            )
    0 // return an integer exit code
