module SqlPen.Tests.Parsing

open NUnit.Framework
open SqlPen
open SqlPen.Parser

[<Test>]
let ``simple parsing test`` = parse "select 0 from Table"
