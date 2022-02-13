namespace SharpLogic

module ConsoleOutput =

    open System.Collections.Generic
    open SharpLogic.Formula
    open SharpLogic.PropositionalLogic
    open Microsoft.FSharp.Core.Printf

    //TODO: write unit tests
    let rec VerboseFormula formula =
        match formula with
        | Var (n) -> n
        | Const (n) -> n.ToString()
        | Disj (n, m) -> $"({VerboseFormula(n)} || {VerboseFormula(m)})"
        | Conj (n, m) -> $"({VerboseFormula(n)} && {VerboseFormula(m)})"
        | Neg (n) -> $"~{VerboseFormula(n)}"
        | Bic (n, m) -> $"({VerboseFormula(n)} <=> {VerboseFormula(m)})"
        | Impl (n, m) -> $"({VerboseFormula(n)} -> {VerboseFormula(m)})"

    //TODO: write unit tests
    //TODO: use PropositionalLogic.CalcFormulaExpression
    let VerboseTableuxCalculus (formulaCalcList: Formula list) (formulaInterpritations: bool list list) : string =
        let sb = new System.Text.StringBuilder()
        let subFormulasVals = new Dictionary<Formula, bool>()

        let _saveFormulaValueAndPrint =
            fun formula value ->
                if not (subFormulasVals.ContainsKey(formula)) then
                    subFormulasVals.Add(formula, value)
                else
                    subFormulasVals.Item(formula) <- value

                bprintf sb $"{value}\t"
                let len = ((VerboseFormula formula).Length / 8)

                for _ = 1 to len do
                    bprintf sb "\t"

        for i = 0 to formulaInterpritations.Length - 1 do
            let rowVarValues = formulaInterpritations.Item i
            let mutable j = 0

            formulaCalcList
            |> List.iter (fun f ->
                match f with
                | Formula.Var (_) ->
                    bprintf sb $"{rowVarValues.Item(j)} \t"
                    j <- j + 1
                | Formula.Neg (Var (x)) ->
                    let index =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let calc = CalcFormula(Formula.Neg(Formula.Const(rowVarValues.Item(index))))
                    _saveFormulaValueAndPrint f calc
                | Formula.Conj (Var (x), Var (y)) ->
                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Conj(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))

                    _saveFormulaValueAndPrint f calc
                | Formula.Disj (Var (x), Var (y)) ->
                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Disj(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))

                    _saveFormulaValueAndPrint f calc
                | Formula.Bic (Var (x), Var (y)) ->
                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Bic(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))

                    _saveFormulaValueAndPrint f calc
                | Formula.Bic (Var (x), y) ->
                    let rightVal = subFormulasVals.GetValueOrDefault(y)

                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let calc = CalcFormula(Bic(Const(rowVarValues.Item(indexX)), Const(rightVal)))
                    _saveFormulaValueAndPrint f calc
                | Formula.Bic (x, Var (y)) ->
                    let leftVal = subFormulasVals.GetValueOrDefault(x)

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc = CalcFormula(Bic(Const(rowVarValues.Item(indexY)), Const(leftVal)))
                    _saveFormulaValueAndPrint f calc
                | Formula.Bic (x, y) ->
                    let leftVal = subFormulasVals.GetValueOrDefault(x)
                    let rightVal = subFormulasVals.GetValueOrDefault(y)
                    let calc = CalcFormula(Bic(Const(rightVal), Const(leftVal)))
                    _saveFormulaValueAndPrint f calc
                | Formula.Impl (Var (x), Var (y)) ->
                    let leftVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let rightVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Impl(Const(rowVarValues.Item(leftVal)), Const(rowVarValues.Item(rightVal))))

                    _saveFormulaValueAndPrint f calc
                | Formula.Impl (x, Var (y)) ->
                    let leftVal = subFormulasVals.GetValueOrDefault(x)

                    let rightVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc = CalcFormula(Impl(Const(leftVal), Const(rowVarValues.Item(rightVal))))
                    _saveFormulaValueAndPrint f calc
                | Formula.Impl (Var (x), y) ->
                    let rightVal = subFormulasVals.GetValueOrDefault(y)

                    let leftVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let calc = CalcFormula(Impl(Const(rowVarValues.Item(leftVal)), Const(rightVal)))
                    _saveFormulaValueAndPrint f calc
                | Formula.Impl (x, y) ->
                    let leftVal = subFormulasVals.GetValueOrDefault(x)
                    let rightVal = subFormulasVals.GetValueOrDefault(y)
                    let calc = CalcFormula(Impl(Const(leftVal), Const(rightVal)))
                    _saveFormulaValueAndPrint f calc
                | _ -> printf "unknown formula %A" f)

            bprintf sb "\r\n"

        sb.ToString()
