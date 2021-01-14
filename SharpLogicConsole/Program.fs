// Learn more about F# at http://fsharp.org

open System
open SharpLogic.Formula

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let expr = Formula.Add(Mul(Const 2, Var "x"), Var "y")
    let expr1 = Formula.Add(Mul(Const 2, Const 0), Var "y")
    let splifiedExpr = simplify expr1
    0 // return an integer exit code
