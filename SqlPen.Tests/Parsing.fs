module SqlPen.Tests.Parsing

open NUnit.Framework
open SqlPen.Tests.Assertions
open SqlPen
open SqlPen.Parser

[<Test>]
let ``simple parsing test``() =
    let expected = {
        Expressions = [IdExpr (Named "Name"); ConstExpr Int]
        Tables = ["Whatever"]
        Filter = ConstExpr Int
    }
    assertEq expected (parse "select Name,0 from Whatever")
    ()
