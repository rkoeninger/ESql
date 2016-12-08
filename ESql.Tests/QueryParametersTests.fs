namespace ESql.Tests

open NUnit.Framework
open Analysis

[<TestFixture>]
[<Category("Unit")>]
type QueryParametersTests() =

    let assertEq (x: 'a) (y: 'a) = Assert.AreEqual(x, y)

    // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
    let people = "People", ["Name", Varchar; "Phone", Varchar; "Email", Varchar]

    [<Test>]
    member this.``where column = literal``() =
        // where Name = 'Guy'
        let clause = BinaryExpr(Eq, IdExpr(Named "Name"), ConstExpr Varchar)
        let query = inferParameters { Condition = clause; Sources = [people] }
        assertEq Map.empty query
        
    [<Test>]
    member this.``where literal = column``() =
        // where 'Guy' = Name
        let clause = BinaryExpr(Eq, ConstExpr Varchar, IdExpr(Named "Name"))
        let query = inferParameters { Condition = clause; Sources = [people] }
        assertEq Map.empty query

    [<Test>]
    member this.``where param = literal``() =
        // where @Id = 1
        let clause = BinaryExpr(Eq, IdExpr(Param "Id"), ConstExpr Int)
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["Id", singleType Int]) query

    [<Test>]
    member this.``where literal = param``() =
        // where 1 = @Id
        let clause = BinaryExpr(Eq, ConstExpr Int, IdExpr(Param "Id"))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["Id", singleType Int]) query

    [<Test>]
    member this.``where params = literals intersection``() =
        // where @X = 1 and @Y = 'hi'
        let clause = BinaryExpr(And, BinaryExpr(Eq, IdExpr(Param "X"), ConstExpr Int), BinaryExpr(Eq, IdExpr(Param "Y"), ConstExpr Varchar))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["X", singleType Int; "Y", singleType Varchar]) query

    [<Test>]
    member this.``where params = literals union``() =
        // where @X = 1 or @Y = 'hi'
        let clause = BinaryExpr(Or, BinaryExpr(Eq, IdExpr(Param "X"), ConstExpr Int), BinaryExpr(Eq, IdExpr(Param "Y"), ConstExpr Varchar))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["X", singleType Int; "Y", singleType Varchar]) query

    [<Test>]
    member this.``where column = param``() =
        // where Name = @YourName
        let clause = BinaryExpr(Eq, IdExpr(Named "Name"), IdExpr(Param "YourName"))
        let query = inferParameters { Condition = clause; Sources = [people] }
        assertEq (Map.ofList ["YourName", singleType Varchar]) query

    [<Test>]
    member this.``where param = column``() =
        // where @YourName = Name
        let clause = BinaryExpr(Eq, IdExpr(Param "YourName"), IdExpr(Named "Name"))
        let query = inferParameters { Condition = clause; Sources = [people] }
        assertEq (Map.ofList ["YourName", singleType Varchar]) query

    [<Test>]
    member this.``where param has union type``() =
        // where @X < 0 or @X = 'hi'
        let clause = BinaryExpr(Or, BinaryExpr(Lt, IdExpr(Param "X"), ConstExpr Int), BinaryExpr(Eq, IdExpr(Param "X"), ConstExpr Varchar))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["X", Limits(Set.ofList [Int; Varchar])]) query

    [<Test>]
    member this.``where param has intersection type``() =
        // where @X < 0 and @X = 'hi'
        let clause = BinaryExpr(And, BinaryExpr(Lt, IdExpr(Param "X"), ConstExpr Int), BinaryExpr(Eq, IdExpr(Param "X"), ConstExpr Varchar))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["X", Limits Set.empty]) query

    [<Test>]
    member this.``where params are compared``() =
        // where @X = @Y
        let clause = BinaryExpr(Eq, IdExpr(Param "X"), IdExpr(Param "Y"))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["X", Any; "Y", Any]) query

    [<Test>]
    [<Ignore("This would require backtracking")>]
    member this.``where params are compared and one is constrained``() =
        // where (@X = @Y) and (@X = 0)
        let clause = BinaryExpr(And, BinaryExpr(Eq, IdExpr(Param "X"), IdExpr(Param "Y")), BinaryExpr(Eq, IdExpr(Param "X"), ConstExpr Int))
        let query = inferParameters { Condition = clause; Sources = [] }
        assertEq (Map.ofList ["X", singleType Int; "Y", singleType Int]) query
