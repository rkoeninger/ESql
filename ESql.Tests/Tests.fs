namespace ESql.Tests

open NUnit.Framework
open Analysis

[<TestFixture>]
[<Category("Unit")>]
type Tests() =

    let assertEq x y = x = y |> Assert.IsTrue

    let expectErr f =
        try
            f() |> ignore
            failwith "Error expected"
        with
            _ -> ()
    
    // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
    let people = "People", ["Name", Varchar; "Phone", Varchar; "Email", Varchar]

    // create table Addresses ( Name varchar(64), City(64) )
    let addresses = "Addresses", ["Name", Varchar; "City", Varchar]

    [<Test>]
    member this.``select``() =
        // select Name, Email from People
        let proj = [IdExpr(Named "Name"); IdExpr(Named "Email")]
        let stmt = analyze { Projection = proj; Sources = [people] }
        assertEq [Some "Name", Varchar; Some "Email", Varchar] stmt
        
    [<Test>]
    member this.``select as``() =
        // select Name as N, Email as E from People
        let proj = [AliasExpr(IdExpr(Named "Name"), "N"); AliasExpr(IdExpr(Named "Email"), "E")]
        let stmt = analyze { Projection = proj; Sources = [people] }
        assertEq [Some "N", Varchar; Some "E", Varchar] stmt
        
    [<Test>]
    member this.``select cast``() =
        // select Name, cast(nvarchar, Email) from People
        let proj = [IdExpr(Named "Name"); CastExpr(IdExpr(Named "Email"), NVarchar)]
        let stmt = analyze { Projection = proj; Sources = [people] }
        assertEq [Some "Name", Varchar; Some "Email", NVarchar] stmt
        
    [<Test>]
    member this.``select cast as``() =
        // select Name, cast(nvarchar, Email) as E from People
        let proj = [IdExpr(Named "Name"); AliasExpr(CastExpr(IdExpr(Named "Email"), NVarchar), "E")]
        let stmt = analyze { Projection = proj; Sources = [people] }
        assertEq [Some "Name", Varchar; Some "E", NVarchar] stmt

    [<Test>]
    member this.``select count``() =
        // select count(*) from People where Age >= 21
        let proj = [CountExpr(IdExpr Star)]
        let stmt = analyze { Projection = proj; Sources = [people] }
        assertEq [None, Int] stmt

    [<Test>]
    member this.``select count as``() =
        // select count(*) as Drinkers from People where Age >= 21
        let proj = [AliasExpr(CountExpr(IdExpr Star), "Drinkers")]
        let stmt = analyze { Projection = proj; Sources = [people] }
        assertEq [Some "Drinkers", Int] stmt

    [<Test>]
    member this.``select *``() =
        // select * from People
        let proj = [IdExpr Star]
        let stmt = analyze { Projection = proj; Sources = [people] }
        assertEq [Some "Name", Varchar; Some "Phone", Varchar; Some "Email", Varchar] stmt

    [<Test>]
    member this.``select Table.* from join``() =
        // select People.* from People
        let proj = [IdExpr(Qualified("People", Star))]
        let stmt = analyze { Projection = proj; Sources = [people; addresses] }
        assertEq [Some "Name", Varchar; Some "Phone", Varchar; Some "Email", Varchar] stmt

    [<Test>]
    member this.``select Table.col from join``() =
        // select People.Name, Addresses.City from People join Addresses on People.Name = Addresses.Name
        let proj = [IdExpr(Qualified("People", Named "Name")); IdExpr(Qualified("Addresses", Named "City"))]
        let stmt = analyze { Projection = proj; Sources = [people; addresses] }
        assertEq [Some "Name", Varchar; Some "City", Varchar] stmt

    [<Test>]
    member this.``select from join``() =
        // select Phone, City from People join Addresses on People.Name = Addresses.Name
        let proj = [IdExpr(Named "Phone"); IdExpr(Named "City")]
        let stmt = analyze { Projection = proj; Sources = [people; addresses] }
        assertEq [Some "Phone", Varchar; Some "City", Varchar] stmt

    [<Test>]
    member this.``select ambiguous name from join``() =
        // select Name, City from People join Addresses on People.Name = Addresses.Name
        let proj = [IdExpr(Named "Name"); IdExpr(Named "City")]
        expectErr(fun () -> analyze { Projection = proj; Sources = [people; addresses] })
