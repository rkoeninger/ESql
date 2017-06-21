module Trilogy.Tests.QueryParameters

open NUnit.Framework
open Trilogy.Tests.Assertions
open Trilogy
open Trilogy.Analysis

// create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
let people = "People", ["Name", Varchar; "Phone", Varchar; "Email", Varchar]

[<Test>]
let ``where column = literal``() =
    // where Name = 'Guy'
    let clause = BinaryExpr(IdExpr(Named "Name"), Eq, ConstExpr Varchar)
    let query = inferParameters { Condition = clause; Sources = [people] }
    assertEq Map.empty query
        
[<Test>]
let ``where literal = column``() =
    // where 'Guy' = Name
    let clause = BinaryExpr(ConstExpr Varchar, Eq, IdExpr(Named "Name"))
    let query = inferParameters { Condition = clause; Sources = [people] }
    assertEq Map.empty query

[<Test>]
let ``where param = literal``() =
    // where @Id = 1
    let clause = BinaryExpr(IdExpr(Param "Id"), Eq, ConstExpr Int)
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["Id", singleType Int]) query

[<Test>]
let ``where literal = param``() =
    // where 1 = @Id
    let clause = BinaryExpr(ConstExpr Int, Eq, IdExpr(Param "Id"))
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["Id", singleType Int]) query

[<Test>]
let ``where params = literals intersection``() =
    // where @X = 1 and @Y = 'hi'
    let clause = BinaryExpr(BinaryExpr(IdExpr(Param "X"), Eq, ConstExpr Int), And, BinaryExpr(IdExpr(Param "Y"), Eq, ConstExpr Varchar))
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["X", singleType Int; "Y", singleType Varchar]) query

[<Test>]
let ``where params = literals union``() =
    // where @X = 1 or @Y = 'hi'
    let clause = BinaryExpr(BinaryExpr(IdExpr(Param "X"), Eq, ConstExpr Int), Or, BinaryExpr(IdExpr(Param "Y"), Eq, ConstExpr Varchar))
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["X", singleType Int; "Y", singleType Varchar]) query

[<Test>]
let ``where column = param``() =
    // where Name = @YourName
    let clause = BinaryExpr(IdExpr(Named "Name"), Eq, IdExpr(Param "YourName"))
    let query = inferParameters { Condition = clause; Sources = [people] }
    assertEq (Map.ofList ["YourName", singleType Varchar]) query

[<Test>]
let ``where param = column``() =
    // where @YourName = Name
    let clause = BinaryExpr(IdExpr(Param "YourName"), Eq, IdExpr(Named "Name"))
    let query = inferParameters { Condition = clause; Sources = [people] }
    assertEq (Map.ofList ["YourName", singleType Varchar]) query

[<Test>]
let ``where param has union type``() =
    // where @X < 0 or @X = 'hi'
    let clause = BinaryExpr(BinaryExpr(IdExpr(Param "X"), Lt, ConstExpr Int), Or, BinaryExpr(IdExpr(Param "X"), Eq, ConstExpr Varchar))
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["X", Limits(Set.ofList [Int; Varchar])]) query

[<Test>]
let ``where param has intersection type``() =
    // where @X < 0 and @X = 'hi'
    let clause = BinaryExpr(BinaryExpr(IdExpr(Param "X"), Lt, ConstExpr Int), And, BinaryExpr(IdExpr(Param "X"), Eq, ConstExpr Varchar))
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["X", Limits Set.empty]) query

[<Test>]
let ``where params are compared``() =
    // where @X = @Y
    let clause = BinaryExpr(IdExpr(Param "X"), Eq, IdExpr(Param "Y"))
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["X", Any; "Y", Any]) query

[<Test; Ignore("This would require backtracking")>]
let ``where params are compared and one is constrained``() =
    // where (@X = @Y) and (@X = 0)
    let clause = BinaryExpr(BinaryExpr(IdExpr(Param "X"), Eq, IdExpr(Param "Y")), And, BinaryExpr(IdExpr(Param "X"), Eq, ConstExpr Int))
    let query = inferParameters { Condition = clause; Sources = [] }
    assertEq (Map.ofList ["X", singleType Int; "Y", singleType Int]) query
