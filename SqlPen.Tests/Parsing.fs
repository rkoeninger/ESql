module SqlPen.Tests.Parsing

open NUnit.Framework
open SqlPen.Tests.Assertions
open SqlPen
open SqlPen.Parser

[<Test>]
let ``simple parsing test``() =
    let expected = {
        Expressions = [IdExpr (Named "Name"); ConstExpr Int]
        Tables = ["T1"; "T2"; "T3"]
        Filter = ConstExpr Int
    }
    assertEq expected (parse "select Name, 0 from T1 join T2 on 0 join T3 on 0")
    assertEq expected (parse "select Name, 0 from T1 join T2 on 0 join T3 on 0 where 0")
    ()
