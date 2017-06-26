module Trilogy.Demo

type Db = Trilogy.LoadQueries<"Sql">

[<EntryPoint>]
let main argv =
    let db = Db()
    //db.Thing <- "hi"
    printfn "%A" db.Thing
    0
