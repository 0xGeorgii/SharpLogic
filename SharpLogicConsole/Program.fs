// Learn more about F# at http://fsharp.org

open System
open SharpLogic.Formula
open SharpLogic.BasicAlgorithms

[<EntryPoint>]
let main argv =
    let frm = Formula.Impl(Conj(Var "P", Var "Q"), Bic(Var "R", Neg(Var "S")))
    let truthTableHeaders = BuildTruthTableHeaders frm |> List.sortBy(fun f -> FormulaCaclDepth f)
    List.iter(fun f -> printf "%A\r\n" f) truthTableHeaders    
    let isFormulaValid = IsFormulaValid frm
    Console.WriteLine($"Formula is valid: [{isFormulaValid}]")
    printf "%s\r\n" "================" |> ignore 
    let verboseTable = truthTableHeaders |> List.map VerboseFormula |> List.iter(fun f -> printf "%s\t" f)
    let vars = 
        truthTableHeaders
            |> List.filter
            (function 
                |Formula.Var(_) -> true
                | _ -> false
            )
    let data = List.init vars.Length (fun _ -> [ true; false ])
    let res = cartList data
    Console.WriteLine("");
    for i = 0 to res.Length - 1 do
        let rowVarValues = res.Item i
        let mutable j = 0
        truthTableHeaders |> List.iter 
            (function f ->
                        match f with
                        | Formula.Var(_) -> 
                            j <- j + 1
                            Console.Write(rowVarValues.Item(j - 1).ToString() + "\t")
                        | Formula.Conj(Var(x), Var(y)) -> Console.Write(CalcFormula(Formula.Conj(Const(true), Const(true))).ToString() + "\t")
                        | _ -> Console.WriteLine("")
            )
    0 // return an integer exit code
