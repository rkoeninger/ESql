module Trilogy.Tests.QueryProjection

open NUnit.Framework
open Assertions
open Trilogy
open Trilogy.Analysis

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
let ``select``() =
    // select Name, Email from People
    let sel = [Unaliased(IdExpr(Named "Name")); Unaliased(IdExpr(Named "Email"))]
    let stmt = inferProjection { Selections = sel; Sources = [people] }
    assertEq [Some "Name", Varchar; Some "Email", Varchar] stmt

[<Test>]
let ``select literal varchar``() =
    // select 'literal'
    let sel = [Unaliased(ConstExpr Varchar)]
    let stmt = inferProjection { Selections = sel; Sources = [] }
    assertEq [None, Varchar] stmt

[<Test>]
let ``select as``() =
    // select Name as N, Email as E from People
    let sel = [Aliased(IdExpr(Named "Name"), "N"); Aliased(IdExpr(Named "Email"), "E")]
    let stmt = inferProjection { Selections = sel; Sources = [people] }
    assertEq [Some "N", Varchar; Some "E", Varchar] stmt

[<Test>]
let ``select cast``() =
    // select Name, cast(nvarchar, Email) from People
    let sel = [Unaliased(IdExpr(Named "Name")); Unaliased(CastExpr(IdExpr(Named "Email"), NVarchar))]
    let stmt = inferProjection { Selections = sel; Sources = [people] }
    assertEq [Some "Name", Varchar; None, NVarchar] stmt

[<Test>]
let ``select cast as``() =
    // select Name, cast(nvarchar, Email) as E from People
    let sel = [Unaliased(IdExpr(Named "Name")); Aliased(CastExpr(IdExpr(Named "Email"), NVarchar), "E")]
    let stmt = inferProjection { Selections = sel; Sources = [people] }
    assertEq [Some "Name", Varchar; Some "E", NVarchar] stmt

[<Test>]
let ``select count``() =
    // select count(Name) from People where Age >= 21
    let sel = [Unaliased(CountExpr(IdExpr(Named "Name")))]
    let stmt = inferProjection { Selections = sel; Sources = [people] }
    assertEq [None, Int] stmt

[<Test>]
let ``select count as``() =
    // select count(Name) as Drinkers from People where Age >= 21
    let sel = [Aliased(CountExpr(IdExpr(Named "Name")), "Drinkers")]
    let stmt = inferProjection { Selections = sel; Sources = [people] }
    assertEq [Some "Drinkers", Int] stmt

[<Test>]
let ``select *``() =
    // select * from People
    let sel = [Star None]
    let stmt = inferProjection { Selections = sel; Sources = [people] }
    assertEq [Some "Name", Varchar; Some "Phone", Varchar; Some "Email", Varchar] stmt

[<Test>]
let ``select Table.*``() =
    // select People.* from People
    let sel = [Star(Some "People")]
    let stmt = inferProjection { Selections = sel; Sources = [people; addresses] }
    assertEq [Some "Name", Varchar; Some "Phone", Varchar; Some "Email", Varchar] stmt

[<Test>]
let ``select Table.col from join``() =
    // select People.Name, Addresses.City from People join Addresses on People.Name = Addresses.Name
    let sel = [Unaliased(IdExpr(Qualified("People", Named "Name"))); Unaliased(IdExpr(Qualified("Addresses", Named "City")))]
    let stmt = inferProjection { Selections = sel; Sources = [people; addresses] }
    assertEq [Some "Name", Varchar; Some "City", Varchar] stmt

[<Test>]
let ``select from join``() =
    // select Phone, City from People join Addresses on People.Name = Addresses.Name
    let sel = [Unaliased(IdExpr(Named "Phone")); Unaliased(IdExpr(Named "City"))]
    let stmt = inferProjection { Selections = sel; Sources = [people; addresses] }
    assertEq [Some "Phone", Varchar; Some "City", Varchar] stmt

[<Test>]
let ``select ambiguous name from join``() =
    // select Name, City from People join Addresses on People.Name = Addresses.Name
    let sel = [Unaliased(IdExpr(Named "Name")); Unaliased(IdExpr(Named "City"))]
    expectErr(fun () -> inferProjection { Selections = sel; Sources = [people; addresses] })
