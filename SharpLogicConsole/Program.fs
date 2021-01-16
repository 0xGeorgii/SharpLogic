// Learn more about F# at http://fsharp.org

open System
open SharpLogic.Formula

[<EntryPoint>]
let main argv =
    let frm = Formula.Disj(Conj(Const true, Var "x"), Var "y")
    let frm1 = Formula.Disj(Conj(Const true, Const false), Var "y")
    let frm2 = Formula.Const true
    let isFormulaValid = IsFormulaValid frm2
    let truthTable = BuildTruthTableHeaders frm
    
    0 // return an integer exit code
