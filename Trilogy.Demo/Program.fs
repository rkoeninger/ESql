module Trilogy.Demo

type Db = Trilogy.LoadQueries<"Sql">

[<EntryPoint>]
let main argv =
    let db = Db()
    printfn "%A" argv
    0
