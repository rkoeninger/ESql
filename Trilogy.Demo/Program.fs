module Trilogy.Demo

type Db = Trilogy.LoadQueries<"Sql">

[<EntryPoint>]
let main argv =
    let db = Db()
    printfn "%A" db.Name
    printfn "%A" db.Phone
    printfn "%A" db.Age
    0
