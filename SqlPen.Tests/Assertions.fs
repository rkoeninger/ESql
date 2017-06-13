module SqlPen.Tests.Assertions

open NUnit.Framework

let assertEq (x: 'a) (y: 'a) = Assert.AreEqual(x, y)
