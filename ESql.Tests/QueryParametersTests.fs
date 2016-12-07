namespace ESql.Tests

open NUnit.Framework
open Analysis

[<TestFixture>]
[<Category("Unit")>]
type QueryParametersTests() =

    let assertEq x y = x = y |> Assert.IsTrue

    [<Test>]
    member this.``where param equals literal``() =
        // where @Id = 1
        let clause = BinaryExpr(Eq, IdExpr(Param "Id"), ConstExpr Int)
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq ["Id", Int] query

    [<Test>]
    member this.``where literal equals param``() =
        // where 1 = @Id
        let clause = BinaryExpr(Eq, ConstExpr Int, IdExpr(Param "Id"))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq ["Id", Int] query
