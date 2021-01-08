// Learn more about F# at http://fsharp.org

open System
open SharpLogic.Expressions

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let expr = Expression.Add(Mul(Const 2, Var "x"), Var "y")
    let expr1 = Expression.Add(Mul(Const 2, Const 0), Var "y")
    let splifiedExpr = simplify expr1
    0 // return an integer exit code
