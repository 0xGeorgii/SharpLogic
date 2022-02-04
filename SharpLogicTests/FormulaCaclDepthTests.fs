namespace SharpLogicTests

open SharpLogic.Formula
open Microsoft.VisualStudio.TestTools.UnitTesting

//TODO: write additional cases
[<TestClass>]
type FormulaCaclDepthTests() =

    [<TestMethod>]
    member _.LengthIs1() =
        let frm = Formula.Var "N"
        let len = CalcFormulaDepth frm
        Assert.AreEqual(1, len)

    [<TestMethod>]
    member _.LengthIs3() =
        let frm = Formula.Disj(Conj(Var "N", Var "M"), Var "H")
        let len = CalcFormulaDepth frm
        Assert.AreEqual(5, len)
