module Trilogy.Demo

type Person = Trilogy.ParseQuery<"
    create table People
    (
        Name varchar,
        Phone varchar,
        Age int
    )
">

[<EntryPoint>]
let main argv =
    let p = Person()
    printfn "%A" p.Name
    printfn "%A" p.Phone
    printfn "%A" p.Age
    0
