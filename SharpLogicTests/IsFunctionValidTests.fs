namespace SharpLogicTests

open SharpLogic.Formula
open Microsoft.VisualStudio.TestTools.UnitTesting

//TODO: write additional cases
[<TestClass>]
type IsFunctionValidTests () =

    [<TestMethod>]
    member _.VariableIsFormula() =
        let frm = Formula.Var "N"
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.NegationIsFormula() =
        let frm = Formula.Neg(Formula.Var "N")
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.ConjunctionIsFormula1() =
        let frm = Formula.Conj(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.ConjunctionIsFormula2() =
        let frm = Formula.Conj(Formula.Const 1, Formula.Var "M")
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.ConjunctionIsFormula3() =
        let frm = Formula.Conj(Formula.Var "N", Formula.Const 1)
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.DisjunctionIsFormula1() =
        let frm = Formula.Disj(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.DisjunctionIsFormula2() =
        let frm = Formula.Disj(Formula.Const 1, Formula.Var "M")
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.DisjunctionIsFormula3() =
        let frm = Formula.Disj(Formula.Var "N", Formula.Const 1)
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.BiconditionalIsFormula() =
        let frm = Formula.Bic(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.ImplicationIsFormula() =
        let frm = Formula.Impl(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaValid frm
        Assert.IsTrue(isFormula)

    [<TestMethod>]
    member _.NegativeCases() =
        let frm = Formula.Const 1
        let isFormula = IsFormulaValid frm
        Assert.IsFalse(isFormula)

