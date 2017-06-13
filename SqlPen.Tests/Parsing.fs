module SqlPen.Tests.Parsing

open NUnit.Framework
open SqlPen.Tests.Assertions
open SqlPen
open SqlPen.Parser

[<Test>]
let ``simple parsing test``() =
    parse "select" |> ignore
    ()
