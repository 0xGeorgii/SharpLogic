// Learn more about F# at http://fsharp.org

open System
open SharpLogic.Formula

[<EntryPoint>]
let main argv =
    let frm = Formula.Impl(Conj(Var "P", Var "Q"), Bic(Var "R", Neg(Var "S")))
    let frm2 = Formula.Const true
    let isFormulaValid = IsFormulaValid frm2
    let truthTable = BuildTruthTableHeaders frm |> List.sortBy(fun f -> FormulaCaclDepth f)
    List.iter(fun f -> printf "%A\r\n" f) truthTable
    printf "%s\r\n" "================" |> ignore 
    let verboseTable = truthTable |> List.map VerboseFormula |> List.iter(fun f -> printf "%s\r\n" f)
    0 // return an integer exit code
