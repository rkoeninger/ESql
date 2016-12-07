namespace ESql.Tests

open NUnit.Framework
open Analysis

[<TestFixture>]
[<Category("Unit")>]
type QueryParametersTests() =

    let assertEq x y = x = y |> Assert.IsTrue

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
