open SharpLogic.Formula

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
    let formulaInterpritations = BuildAllFormulasInterpritations formulaCalcList
    let output =  VerboseTableuxCalculus formulaCalcList formulaInterpritations
    printf "%s" output

    0 // return an integer exit code
