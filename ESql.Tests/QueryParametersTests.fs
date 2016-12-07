namespace ESql.Tests

open NUnit.Framework
open Analysis

[<TestFixture>]
[<Category("Unit")>]
type QueryParametersTests() =

    let assertEq x y = x = y |> Assert.IsTrue
    
    // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
    let people = "People", ["Name", Varchar; "Phone", Varchar; "Email", Varchar]

    [<Test>]
    member this.``where column = literal``() =
        // where Name = 'Guy'
        let clause = BinaryExpr(Eq, IdExpr(Named "Name"), ConstExpr Varchar)
        let query = inferParameters { Condition = clause; Sources = [people] }
        assertEq Map.empty query

    [<Test>]
    member this.``where param = literal``() =
        // where @Id = 1
        let clause = BinaryExpr(Eq, IdExpr(Param "Id"), ConstExpr Int)
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["Id", Int]) query

    [<Test>]
    member this.``where literal = param``() =
        // where 1 = @Id
        let clause = BinaryExpr(Eq, ConstExpr Int, IdExpr(Param "Id"))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["Id", Int]) query

    [<Test>]
    member this.``where params = literals``() =
        // where @X = 1 and @Y = 'hi'
        let clause = BinaryExpr(And, BinaryExpr(Eq, IdExpr(Param "X"), ConstExpr Int), BinaryExpr(Eq, IdExpr(Param "Y"), ConstExpr Varchar))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["X", Int; "Y", Varchar]) query

    [<Test>]
    member this.``where column = param``() =
        // where Name = @YourName
        let clause = BinaryExpr(Eq, IdExpr(Named "Name"), IdExpr(Param "YourName"))
        let query = inferParameters { Condition = clause; Sources = [people] }
        assertEq (Map.ofList ["YourName", Varchar]) query
