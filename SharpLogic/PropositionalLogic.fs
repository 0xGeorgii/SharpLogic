namespace SharpLogic

module PropositionalLogic =

    open SharpLogic.BasicAlgorithms
    open Microsoft.FSharp.Core.Printf
    open SharpLogic.Formula
    open System.Collections.Generic

    //TODO: write unit tests
    //FIXME: handle a sitation when a Var appears >= 1 in a formula. formula interpritations should be list<tuple<Formula, bool>>
    let BuildAllFormulasInterpritations formulaCalcList =
        let vars =
            formulaCalcList
            |> List.filter (function
                | Formula.Var (_) -> true
                | _ -> false)

        let data = List.init vars.Length (fun _ -> [ true; false ])
        cartList data

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
    //TODO: write a wiki page
    //TODO: think about naming
    let CalcFormulaExpression (formulaCalcList: Formula list) (formulaInterpritations: bool list list) =
        let subFormulasVals = new List<Dictionary<Formula, bool>>(formulaInterpritations.Length)

        let _saveFormulaValueAndValidate =
            fun formula value index->
                if not (subFormulasVals.Item(index).ContainsKey(formula)) then
                    subFormulasVals.Item(index).Add(formula, value)
                else
                    subFormulasVals.Item(index).Item(formula) <- value

        for i = 0 to formulaInterpritations.Length - 1 do
            let rowVarValues = formulaInterpritations.Item i
            let mutable j = 0
            subFormulasVals.Add(new Dictionary<Formula, bool>())

            formulaCalcList
            |> List.iter (fun f ->
                match f with
                | Formula.Var (_) ->
                    _saveFormulaValueAndValidate f (rowVarValues.Item(j)) i
                    j <- j + 1
                | Formula.Neg (Var (x)) ->
                    let index =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let calc = CalcFormula(Formula.Neg(Formula.Const(rowVarValues.Item(index))))
                    _saveFormulaValueAndValidate f calc i
                | Formula.Conj (Var (x), Var (y)) ->
                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Conj(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))

                    _saveFormulaValueAndValidate f calc i
                | Formula.Disj (Var (x), Var (y)) ->
                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Disj(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))

                    _saveFormulaValueAndValidate f calc i
                | Formula.Bic (Var (x), Var (y)) ->
                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Bic(Const(rowVarValues.Item(indexX)), Const(rowVarValues.Item(indexY))))

                    _saveFormulaValueAndValidate f calc i
                | Formula.Bic (Var (x), y) ->
                    let rightVal = subFormulasVals.Item(i).GetValueOrDefault(y)

                    let indexX =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let calc = CalcFormula(Bic(Const(rowVarValues.Item(indexX)), Const(rightVal)))
                    _saveFormulaValueAndValidate f calc i
                | Formula.Bic (x, Var (y)) ->
                    let leftVal = subFormulasVals.Item(i).GetValueOrDefault(x)

                    let indexY =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc = CalcFormula(Bic(Const(rowVarValues.Item(indexY)), Const(leftVal)))
                    _saveFormulaValueAndValidate f calc i
                | Formula.Bic (x, y) ->
                    let leftVal = subFormulasVals.Item(i).GetValueOrDefault(x)
                    let rightVal = subFormulasVals.Item(i).GetValueOrDefault(y)
                    let calc = CalcFormula(Bic(Const(rightVal), Const(leftVal)))
                    _saveFormulaValueAndValidate f calc i
                | Formula.Impl (Var (x), Var (y)) ->
                    let leftVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let rightVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc =
                        CalcFormula(Impl(Const(rowVarValues.Item(leftVal)), Const(rowVarValues.Item(rightVal))))

                    _saveFormulaValueAndValidate f calc i
                | Formula.Impl (x, Var (y)) ->
                    let leftVal = subFormulasVals.Item(i).GetValueOrDefault(x)

                    let rightVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(y))

                    let calc = CalcFormula(Impl(Const(leftVal), Const(rowVarValues.Item(rightVal))))
                    _saveFormulaValueAndValidate f calc i
                | Formula.Impl (Var (x), y) ->
                    let rightVal = subFormulasVals.Item(i).GetValueOrDefault(y)

                    let leftVal =
                        formulaCalcList
                        |> List.findIndex (fun h -> h = Var(x))

                    let calc = CalcFormula(Impl(Const(rowVarValues.Item(leftVal)), Const(rightVal)))
                    _saveFormulaValueAndValidate f calc i
                | Formula.Impl (x, y) ->
                    let leftVal = subFormulasVals.Item(i).GetValueOrDefault(x)
                    let rightVal = subFormulasVals.Item(i).GetValueOrDefault(y)
                    let calc = CalcFormula(Impl(Const(leftVal), Const(rightVal)))
                    _saveFormulaValueAndValidate f calc i
                | _ -> printf "unknown formula %A" f)
        
        subFormulasVals
        |> List.ofSeq

    //TODO: write unit tests
    //TODO: write wiki page
    let IsFormulaConsistent formula =
        let formulaCalcList =
            BuildFormulaCalcList formula
            |> List.sortBy (fun f -> CalcFormulaDepth f)
        let formulaInterpritations = BuildAllFormulasInterpritations formulaCalcList
        let formulaCalculationResult = CalcFormulaExpression formulaCalcList formulaInterpritations
        let mutable isFormulaConsistent = false
        formulaCalculationResult
        |> List.iter (fun (f: Dictionary<Formula, bool>) ->
            match f.TryGetValue formula with
            | true, value -> if value then isFormulaConsistent <- false else isFormulaConsistent <- isFormulaConsistent
            | false, _ -> isFormulaConsistent <- isFormulaConsistent
        )
        isFormulaConsistent
