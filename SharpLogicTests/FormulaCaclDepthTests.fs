namespace SharpLogicTests

open SharpLogic.Formula
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit

//TODO: write additional cases
[<TestClass>]
type FormulaCaclDepthTests() =

    [<TestMethod>]
    member _.LengthIs1() =
        let frm = Formula.Var "N"
        let len = CalcFormulaDepth frm
        len |> should equal 1

    [<TestMethod>]
    member _.LengthIs3() =
        let frm = Formula.Disj(Conj(Var "N", Var "M"), Var "H")
        let len = CalcFormulaDepth frm
        len |> should equal 5
