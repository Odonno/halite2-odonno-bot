module MathTests

open System
open Xunit

[<Fact>]
let ``Calculate cosinus of 0`` () =
    let result = cos 0.0
    Assert.Equal(1.0, result)

[<Fact>]
let ``Calculate cosinus of Pi / 2`` () =
    let result = Math.Round( (cos (Math.PI / 2.0)), 10 )
    Assert.Equal(0.0, result)

[<Fact>]
let ``Calculate cosinus of Pi`` () =
    let result = cos Math.PI
    Assert.Equal(-1.0, result)