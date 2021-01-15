// Learn more about F# at http://fsharp.org

open System
open SharpLogic.Formula

[<EntryPoint>]
let main argv =
    let frm = Formula.Disj(Conj(Const 2, Var "x"), Var "y")
    let frm1 = Formula.Disj(Conj(Const 2, Const 0), Var "y")
    let frm2 = Formula.Const 1
    let splifiedExpr = Simplify frm1
    let isFormulaValid = IsFormulaValid frm2
    0 // return an integer exit code
