module Trilogy.Tests.Parsing

open NUnit.Framework
open Trilogy.Tests.Assertions
open Trilogy
open Trilogy.Parser

[<Test>]
let ``select statement``() =
    let expected = SelectStatement {
        Expressions = [IdExpr (Named "Name"); ConstExpr Int]
        Tables = ["T1"; "T2"; "T3"]
        Filter = ConstExpr Int
    }
    assertEq expected (parse "select Name, 0 from T1 join T2 on 0 join T3 on 0")
    assertEq expected (parse "select Name, 0 from T1 join T2 on 0 join T3 on 0 where 0")

[<Test>]
let ``insert statement``() =
    let expected = InsertStatement {
        Table = "Tbl"
        Columns = ["X"; "Y"; "Z"]
        Values = [ConstExpr Int; ConstExpr Varchar; ConstExpr Int]
    }
    assertEq expected (parse "insert into Tbl (X, Y, Z) values (1, 'a', 53434)")

[<Test>]
let ``update statement``() =
    let expected = UpdateStatement {
        Table = "Tbl"
        Assignments = ["X", ConstExpr Int; "Y", ConstExpr Varchar]
        Filter = ConstExpr Int
    }
    assertEq expected (parse "update Tbl set X = 0, Y = 'a'")
    assertEq expected (parse "update Tbl set X = 0, Y = 'a' where 0")

[<Test>]
let ``delete statement``() =
    let expected = InsertStatement {
        Table = "Tbl"
        Columns = ["X"; "Y"]
        Values = [ConstExpr Int; ConstExpr Varchar]
    }
    assertEq expected (parse "insert into Tbl (X, Y) values (0, 'a')")

[<Test>]
let ``create table statment``() =
    let expected = CreateStatement {
        Name = "Thingy"
        Columns = ["X", Int; "Y", Varchar]
    }
    assertEq expected (parse "create table Thingy ( X int, Y varchar )")

[<Test>]
let ``multiple statements``() = ()
