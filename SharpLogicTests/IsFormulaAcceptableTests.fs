namespace SharpLogicTests

open SharpLogic.Formula
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit

//TODO: write additional cases
[<TestClass>]
type IsFormulaAcceptableTests() =

    [<TestMethod>]
    member _.VariableIsFormula() =
        let frm = Formula.Var "N"
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.NegationIsFormula() =
        let frm = Formula.Neg(Formula.Var "N")
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.ConjunctionIsFormula1() =
        let frm = Formula.Conj(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.ConjunctionIsFormula2() =
        let frm = Formula.Conj(Formula.Const true, Formula.Var "M")
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.ConjunctionIsFormula3() =
        let frm = Formula.Conj(Formula.Var "N", Formula.Const false)
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.DisjunctionIsFormula1() =
        let frm = Formula.Disj(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.DisjunctionIsFormula2() =
        let frm = Formula.Disj(Formula.Const true, Formula.Var "M")
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.DisjunctionIsFormula3() =
        let frm = Formula.Disj(Formula.Var "N", Formula.Const true)
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.BiconditionalIsFormula() =
        let frm = Formula.Bic(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.ImplicationIsFormula() =
        let frm = Formula.Impl(Formula.Var "N", Formula.Var "M")
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be True

    [<TestMethod>]
    member _.NegativeCases() =
        let frm = Formula.Const true
        let isFormula = IsFormulaAcceptable frm
        isFormula |> should be False
