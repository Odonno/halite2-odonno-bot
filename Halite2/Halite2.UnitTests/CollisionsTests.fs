module CollisionsTests

open System
open Xunit
open Collisions

[<Fact>]
let ``Calculate distance between the same position`` () =
    let distance = calculateDistanceTo { X = 0.0; Y = 0.0; } { X = 0.0; Y = 0.0; }

    Assert.Equal(0.0, distance)

[<Fact>]
let ``Calculate distance between two integer positions`` () =
    let distance = calculateDistanceTo { X = 0.0; Y = 0.0; } { X = 2.0; Y = 0.0; }

    Assert.Equal(2.0, distance)
    
[<Fact>]
let ``Calculate distance between two float positions`` () =
    let distance = calculateDistanceTo { X = 1.5; Y = 1.5; } { X = 2.3; Y = 8.4; }

    Assert.Equal(6.95, Math.Round(distance, 2))
    
[<Fact>]
let ``Two circles should collide`` () =
    let c1 = { Position = { X = 0.0; Y = 0.0; }; Radius = 2.5; }
    let c2 = { Position = { X = 5.0; Y = 0.0; }; Radius = 2.5; }

    Assert.True(circlesCollide c1 c2)
    
[<Fact>]
let ``Two circles should not collide`` () =
    let c1 = { Position = { X = 0.0; Y = 0.0; }; Radius = 2.5; }
    let c2 = { Position = { X = 5.1; Y = 0.0; }; Radius = 2.5; }

    Assert.False(circlesCollide c1 c2)

[<Fact>]
let ``Position should be in the circle`` () =
    let c1 = { Position = { X = 0.0; Y = 0.0; }; Radius = 2.5; }
    let p1 = { X = 2.0; Y = 0.0; }

    Assert.True(isInCircle p1 c1)
    
[<Fact>]
let ``Position should not be in the circle`` () =
    let c1 = { Position = { X = 0.0; Y = 0.0; }; Radius = 2.5; }
    let p1 = { X = 5.0; Y = 0.0; }

    Assert.False(isInCircle p1 c1)