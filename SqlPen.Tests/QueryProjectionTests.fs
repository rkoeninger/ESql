﻿namespace SqlPen.Tests

open NUnit.Framework
open SqlPen
open SqlPen.Analysis

[<TestFixture>]
[<Category("Unit")>]
type QueryProjectionTests() =

    let assertEq (x: 'a) (y: 'a) = Assert.AreEqual(x, y)

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
        let sel = [IdExpr(Named "Name"); IdExpr(Named "Email")]
        let stmt = inferProjection { Selections = sel; Sources = [people] }
        assertEq [Some "Name", Varchar; Some "Email", Varchar] stmt
        
    [<Test>]
    member this.``select literal varchar``() =
        // select 'literal'
        let sel = [ConstExpr Varchar]
        let stmt = inferProjection { Selections = sel; Sources = [] }
        assertEq [None, Varchar] stmt

    [<Test>]
    member this.``select as``() =
        // select Name as N, Email as E from People
        let sel = [AliasExpr(IdExpr(Named "Name"), "N"); AliasExpr(IdExpr(Named "Email"), "E")]
        let stmt = inferProjection { Selections = sel; Sources = [people] }
        assertEq [Some "N", Varchar; Some "E", Varchar] stmt

    [<Test>]
    member this.``select cast``() =
        // select Name, cast(nvarchar, Email) from People
        let sel = [IdExpr(Named "Name"); CastExpr(IdExpr(Named "Email"), NVarchar)]
        let stmt = inferProjection { Selections = sel; Sources = [people] }
        assertEq [Some "Name", Varchar; None, NVarchar] stmt

    [<Test>]
    member this.``select cast as``() =
        // select Name, cast(nvarchar, Email) as E from People
        let sel = [IdExpr(Named "Name"); AliasExpr(CastExpr(IdExpr(Named "Email"), NVarchar), "E")]
        let stmt = inferProjection { Selections = sel; Sources = [people] }
        assertEq [Some "Name", Varchar; Some "E", NVarchar] stmt

    [<Test>]
    member this.``select count``() =
        // select count(*) from People where Age >= 21
        let sel = [CountExpr(IdExpr Star)]
        let stmt = inferProjection { Selections = sel; Sources = [people] }
        assertEq [None, Int] stmt

    [<Test>]
    member this.``select count as``() =
        // select count(*) as Drinkers from People where Age >= 21
        let sel = [AliasExpr(CountExpr(IdExpr Star), "Drinkers")]
        let stmt = inferProjection { Selections = sel; Sources = [people] }
        assertEq [Some "Drinkers", Int] stmt

    [<Test>]
    member this.``select *``() =
        // select * from People
        let sel = [IdExpr Star]
        let stmt = inferProjection { Selections = sel; Sources = [people] }
        assertEq [Some "Name", Varchar; Some "Phone", Varchar; Some "Email", Varchar] stmt

    [<Test>]
    member this.``select Table.*``() =
        // select People.* from People
        let sel = [IdExpr(Qualified("People", Star))]
        let stmt = inferProjection { Selections = sel; Sources = [people; addresses] }
        assertEq [Some "Name", Varchar; Some "Phone", Varchar; Some "Email", Varchar] stmt

    [<Test>]
    member this.``select Table.col from join``() =
        // select People.Name, Addresses.City from People join Addresses on People.Name = Addresses.Name
        let sel = [IdExpr(Qualified("People", Named "Name")); IdExpr(Qualified("Addresses", Named "City"))]
        let stmt = inferProjection { Selections = sel; Sources = [people; addresses] }
        assertEq [Some "Name", Varchar; Some "City", Varchar] stmt

    [<Test>]
    member this.``select from join``() =
        // select Phone, City from People join Addresses on People.Name = Addresses.Name
        let sel = [IdExpr(Named "Phone"); IdExpr(Named "City")]
        let stmt = inferProjection { Selections = sel; Sources = [people; addresses] }
        assertEq [Some "Phone", Varchar; Some "City", Varchar] stmt

    [<Test>]
    member this.``select ambiguous name from join``() =
        // select Name, City from People join Addresses on People.Name = Addresses.Name
        let sel = [IdExpr(Named "Name"); IdExpr(Named "City")]
        expectErr(fun () -> inferProjection { Selections = sel; Sources = [people; addresses] })
