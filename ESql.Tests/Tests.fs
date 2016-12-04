namespace ESql.Tests

open NUnit.Framework
open Analysis

[<TestFixture>]
[<Category("Unit")>]
type Tests() =

    let assertEq x y = x = y |> Assert.IsTrue

    [<Test>]
    member this.``select``() =
        // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
        let cols = ["Name", Varchar; "Phone", Varchar; "Email", Varchar]
        // select Name, Email from People
        let proj = analyze { Projection = [IdExpr(Named "Name"); IdExpr(Named "Email")]; Source = cols }
        assertEq [Some "Name", Varchar; Some "Email", Varchar] proj
        
    [<Test>]
    member this.``select as``() =
        // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
        let cols = ["Name", Varchar; "Phone", Varchar; "Email", Varchar]
        // select Name as N, Email as E from People
        let proj = analyze { Projection = [AliasExpr(IdExpr(Named "Name"), "N"); AliasExpr(IdExpr(Named "Email"), "E")]; Source = cols }
        assertEq [Some "N", Varchar; Some "E", Varchar] proj
        
    [<Test>]
    member this.``select cast``() =
        // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
        let cols = ["Name", Varchar; "Phone", Varchar; "Email", Varchar]
        // select Name, cast(nvarchar, Email) from People
        let proj = analyze { Projection = [IdExpr(Named "Name"); CastExpr(IdExpr(Named "Email"), NVarchar)]; Source = cols }
        assertEq [Some "Name", Varchar; Some "Email", NVarchar] proj
        
    [<Test>]
    member this.``select cast as``() =
        // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
        let cols = ["Name", Varchar; "Phone", Varchar; "Email", Varchar]
        // select Name, cast(nvarchar, Email) as E from People
        let proj = analyze { Projection = [IdExpr(Named "Name"); AliasExpr(CastExpr(IdExpr(Named "Email"), NVarchar), "E")]; Source = cols }
        assertEq [Some "Name", Varchar; Some "E", NVarchar] proj

    [<Test>]
    member this.``select count``() =
        // create table People ( Name varchar(64), Age int )
        let cols = ["Name", Varchar; "Age", Int]
        // select count(*) from People where Age >= 21
        let proj = analyze { Projection = [CountExpr(IdExpr Star)]; Source = cols }
        assertEq [None, Int] proj

    [<Test>]
    member this.``select count as``() =
        // create table People ( Name varchar(64), Age int )
        let cols = ["Name", Varchar; "Age", Int]
        // select count(*) as Drinkers from People where Age >= 21
        let proj = analyze { Projection = [AliasExpr(CountExpr(IdExpr Star), "Drinkers")]; Source = cols }
        assertEq [Some "Drinkers", Int] proj

    [<Test>]
    member this.``select *``() =
        // create table People ( Name varchar(64), Phone varchar(16), Email varchar(32) )
        let cols = ["Name", Varchar; "Phone", Varchar; "Email", Varchar]
        // select * from People
        let proj = analyze { Projection = [IdExpr Star]; Source = cols }
        assertEq [Some "Name", Varchar; Some "Phone", Varchar; Some "Email", Varchar] proj
