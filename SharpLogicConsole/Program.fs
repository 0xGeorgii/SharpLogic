// Learn more about F# at http://fsharp.org

open System
open SharpLogic.Formula

[<EntryPoint>]
let main argv =
    let frm = Formula.Add(Mul(Const 2, Var "x"), Var "y")
    let frm1 = Formula.Add(Mul(Const 2, Const 0), Var "y")
    let frm2 = Formula.Const 1
    let splifiedExpr = simplify frm1
    let isFormulaValid = isFormulaValid frm2
    0 // return an integer exit code
