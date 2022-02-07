open SharpLogic.Formula
open SharpLogic.PropositionalLogic
open SharpLogic.ConsoleOutput

[<EntryPoint>]
let main argv =
    let frm = Formula.Impl(Conj(Var "P", Var "Q"), Bic(Var "R", Neg(Var "S")))
    printf "%s\n" (VerboseFormula frm)

    let formulaCalcList =
        BuildFormulaCalcList frm
        |> List.sortBy (fun f -> CalcFormulaDepth f)

    List.iter (fun f -> printf "%A\r\n" f) formulaCalcList
    let isFormulaAcceptable = IsFormulaAcceptable frm
    printf $"Formula is acceptable: [{isFormulaAcceptable}]\n"
    printf "%s\r\n" "================" |> ignore

    formulaCalcList
    |> List.map VerboseFormula
    |> List.iter (fun f -> printf "%s\t" f)

    //TODO: smartly include it into the expression above
    printf "\r\n"

    let formulaInterpritations = BuildAllFormulasInterpritations formulaCalcList
    let output = VerboseTableuxCalculus formulaCalcList formulaInterpritations
    printf "%s" output

    0 // return an integer exit code
