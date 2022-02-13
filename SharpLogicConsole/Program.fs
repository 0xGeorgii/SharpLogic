open SharpLogic.Formula
open SharpLogic.PropositionalLogic
open SharpLogic.ConsoleOutput

[<EntryPoint>]
let main argv =
    let formula = Formula.Impl(Conj(Var "P", Var "Q"), Bic(Var "R", Neg(Var "S")))
    printf "%s\n" (VerboseFormula formula)

    let formulaCalcList =
        BuildFormulaCalcList formula
        |> List.sortBy (fun f -> CalcFormulaDepth f)

    List.iter (fun f -> printf "%A\r\n" f) formulaCalcList
    let isFormulaAcceptable = IsFormulaAcceptable formula
    printf $"Formula is acceptable: [{isFormulaAcceptable}]\n"
    printf "%s\r\n" "================" |> ignore
    
    let consistenFormula = Formula.Conj(Impl(Var "P", Var "Q"), Conj(Var "P", Neg(Var "Q")))
    printf "%s\n" (VerboseFormula consistenFormula)
    let ifFormulaConsistent = IsFormulaConsistent consistenFormula
    printf $"Formula is consistent: [{ifFormulaConsistent}]\n"
    printf "%s\r\n" "================" |> ignore

    formulaCalcList
    |> List.map VerboseFormula
    |> List.iter (fun f -> printf "%s\t" f)

    //TODO: smartly include it into the expression above
    printf "\r\n"

    let formulaInterpritations = BuildAllFormulasInterpritations formulaCalcList
    //let output = VerboseTableuxCalculus formulaCalcList formulaInterpritations
    //printf "%s" output

    0 // return an integer exit code
