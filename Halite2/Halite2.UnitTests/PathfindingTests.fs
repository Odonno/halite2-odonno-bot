module PathfindingTests

open Xunit
open Pathfinding

[<Fact>]
let ``Try go forward returns empty path if same position`` () =
    let heatMap = { Entities = [||] }

    let pathOption = tryGoForward heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 0.0; Y = 0.0; })

    Assert.True(pathOption.IsSome)
    Assert.Equal(0, pathOption.Value.Length)

[<Fact>]
let ``Try go forward returns empty path if too close`` () =
    let heatMap = { Entities = [||] }

    let pathOption = tryGoForward heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 0.2; Y = 0.0; })

    Assert.True(pathOption.IsSome)
    Assert.Equal(0, pathOption.Value.Length)

[<Fact>]
let ``Try go forward returns simple path if no obstacle`` () =
    let heatMap = { Entities = [||] }

    let pathOption = tryGoForward heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 2.0; Y = 0.0; })

    Assert.True(pathOption.IsSome)
    Assert.Equal(1, pathOption.Value.Length)

    Assert.Equal(2, pathOption.Value.[0].Speed)
    Assert.Equal(0, pathOption.Value.[0].Angle)

[<Fact>]
let ``Try go forward returns one path if no obstacle`` () =
    let heatMap = { Entities = [||] }

    let pathOption = tryGoForward heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 7.9; Y = 0.0; })

    Assert.True(pathOption.IsSome)
    Assert.Equal(1, pathOption.Value.Length)

    Assert.Equal(7, pathOption.Value.[0].Speed)
    Assert.Equal(0, pathOption.Value.[0].Angle)

[<Fact>]
let ``Try go forward returns two paths if no obstacle`` () =
    let heatMap = { Entities = [||] }

    let pathOption = tryGoForward heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 8.1; Y = 0.0; })

    Assert.True(pathOption.IsSome)
    Assert.Equal(2, pathOption.Value.Length)

    Assert.Equal(7, pathOption.Value.[0].Speed)
    Assert.Equal(0, pathOption.Value.[0].Angle)

    Assert.Equal(1, pathOption.Value.[1].Speed)
    Assert.Equal(0, pathOption.Value.[1].Angle)