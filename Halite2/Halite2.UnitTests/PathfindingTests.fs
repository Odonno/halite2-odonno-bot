module PathfindingTests

open Xunit
open Halite
open Pathfinding
open Constants
open Collisions

[<Fact>]
let ``Try go forward returns no path if obstacle`` () =
    let ship =
        {
            Id = 1;
            OwnerId = 0;
            Circle = 
                {
                    Position = { X = 0.0; Y = 0.0; };
                    Radius = 0.5;
                };
            Health = 250;
        };
    let shipObstacle =
        {
            Id = 2;
            OwnerId = 0;
            Circle = 
                {
                    Position = { X = 1.0; Y = 0.0; };
                    Radius = 0.5;
                };
            Health = 250;
        }
    let heatMap = List.init 10 (fun i -> { Turn = i + 1; Entities = [| shipObstacle |]; })
    let angle = (calculateAngleTo ({ X = 0.0; Y = 0.0; }) ({ X = 2.0; Y = 0.0; })) |> int

    let pathOrObstacle = goToOrReturnFirstObstacle 1 ship heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 2.0; Y = 0.0; }) angle 0.0 SHIP_RADIUS

    match pathOrObstacle with
        | PathOrObstacle.Obstacle o -> Assert.Equal(2, o.Id)
        | _ -> Assert.False(true)

[<Fact>]
let ``Try go forward returns empty path if same position`` () =
    let ship =
        {
            Id = 1;
            OwnerId = 0;
            Circle = 
                {
                    Position = { X = 0.0; Y = 0.0; };
                    Radius = 0.5;
                };
            Health = 250;
        };
    let heatMap = List.init 10 (fun i -> { Turn = i + 1; Entities = [||]; })
    let angle = (calculateAngleTo ({ X = 0.0; Y = 0.0; }) ({ X = 0.0; Y = 0.0; })) |> int

    let pathOrObstacle = goToOrReturnFirstObstacle 1 ship heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 0.0; Y = 0.0; }) angle 0.0 SHIP_RADIUS
    
    match pathOrObstacle with
        | Path p -> Assert.Equal(0, p.Length)
        | _ -> Assert.False(true)

[<Fact>]
let ``Try go forward returns empty path if too close`` () =
    let ship =
        {
            Id = 1;
            OwnerId = 0;
            Circle = 
                {
                    Position = { X = 0.0; Y = 0.0; };
                    Radius = 0.5;
                };
            Health = 250;
        };
    let heatMap = List.init 10 (fun i -> { Turn = i + 1; Entities = [||]; })
    let angle = (calculateAngleTo ({ X = 0.0; Y = 0.0; }) ({ X = 0.2; Y = 0.0; })) |> int

    let pathOrObstacle = goToOrReturnFirstObstacle 1 ship heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 0.2; Y = 0.0; }) angle 0.0 SHIP_RADIUS
        
    match pathOrObstacle with
        | Path p -> Assert.Equal(0, p.Length)
        | _ -> Assert.False(true)

[<Fact>]
let ``Try go forward returns simple path if no obstacle`` () =
    let ship =
        {
            Id = 1;
            OwnerId = 0;
            Circle = 
                {
                    Position = { X = 0.0; Y = 0.0; };
                    Radius = 0.5;
                };
            Health = 250;
        };
    let heatMap = List.init 10 (fun i -> { Turn = i + 1; Entities = [||]; })
    let angle = (calculateAngleTo ({ X = 0.0; Y = 0.0; }) ({ X = 2.0; Y = 0.0; })) |> int

    let pathOrObstacle = goToOrReturnFirstObstacle 1 ship heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 2.0; Y = 0.0; }) angle 0.0 SHIP_RADIUS
    
    match pathOrObstacle with
        | Path p -> 
            (
                Assert.Equal(1, p.Length)

                Assert.Equal(2, p.[0].Speed)
                Assert.Equal(0, p.[0].Angle)
            )
        | _ -> Assert.False(true)

[<Fact>]
let ``Try go forward returns one path if no obstacle`` () =
    let ship =
        {
            Id = 1;
            OwnerId = 0;
            Circle = 
                {
                    Position = { X = 0.0; Y = 0.0; };
                    Radius = 0.5;
                };
            Health = 250;
        };
    let heatMap = List.init 10 (fun i -> { Turn = i + 1; Entities = [||]; })
    let angle = (calculateAngleTo ({ X = 0.0; Y = 0.0; }) ({ X = 7.9; Y = 0.0; })) |> int

    let pathOrObstacle = goToOrReturnFirstObstacle 1 ship heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 7.9; Y = 0.0; }) angle SHIP_RADIUS (SHIP_RADIUS*2.0)
    
    match pathOrObstacle with
        | Path p -> 
            (
                Assert.Equal(1, p.Length)

                Assert.Equal(7, p.[0].Speed)
                Assert.Equal(0, p.[0].Angle)
            )
        | _ -> Assert.False(true)

[<Fact>]
let ``Try go forward returns two paths if no obstacle`` () =
    let ship =
        {
            Id = 1;
            OwnerId = 0;
            Circle = 
                {
                    Position = { X = 1.0; Y = 0.0; };
                    Radius = 0.5;
                };
            Health = 250;
        };
    let heatMap = List.init 10 (fun i -> { Turn = i + 1; Entities = [||]; })
    let angle = (calculateAngleTo ({ X = 0.0; Y = 0.0; }) ({ X = 8.1; Y = 0.0; })) |> int

    let pathOrObstacle = goToOrReturnFirstObstacle 1 ship heatMap ({ X = 0.0; Y = 0.0; }) ({ X = 8.1; Y = 0.0; }) angle 0.0 SHIP_RADIUS
    
    match pathOrObstacle with
        | Path p -> 
            (
                Assert.Equal(2, p.Length)

                Assert.Equal(7, p.[0].Speed)
                Assert.Equal(0, p.[0].Angle)

                Assert.Equal(1, p.[1].Speed)
                Assert.Equal(0, p.[0].Angle)
            )
        | _ -> Assert.False(true)

[<Fact>]
let ``Entities colliding and self should return only self if only one entity`` () =
    let entities = 
        [| 
            {
                Id = 1;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 1.0; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            }
        |]
    let heatMapTurn = { Turn = 1; Entities = entities; }

    let resultEntities = entitiesCollidingAndSelf heatMapTurn entities.[0] 0.0

    Assert.Equal(1, resultEntities.Length)
    Assert.Equal(1, resultEntities.[0].Id)

[<Fact>]
let ``Entities colliding and self should return only self if not colliding entity`` () =
    let entities = 
        [| 
            {
                Id = 1;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 1.0; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            };
            {
                Id = 2;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 5.0; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            };
            {
                Id = 3;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 0.0; Y = 5.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            }
        |]
    let heatMapTurn = { Turn = 1; Entities = entities; }

    let resultEntities = entitiesCollidingAndSelf heatMapTurn entities.[0] 0.0

    Assert.Equal(1, resultEntities.Length)
    Assert.Equal(1, resultEntities.[0].Id)

[<Fact>]
let ``Entities colliding and self should return two entities including self if one collide another one`` () =
    let entities = 
        [| 
            {
                Id = 1;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 1.0; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            };
            {
                Id = 2;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 5.0; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            };
            {
                Id = 3;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 0.5; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            }
        |]
    let heatMapTurn = { Turn = 1; Entities = entities; }

    let resultEntities = entitiesCollidingAndSelf heatMapTurn entities.[0] 0.0

    Assert.Equal(2, resultEntities.Length)
    Assert.Equal(1, resultEntities.[1].Id)
    Assert.Equal(3, resultEntities.[0].Id)

[<Fact>]
let ``Entities colliding and self should return many entities`` () =
    let entities = 
        [| 
            {
                Id = 1;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 1.0; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            };
            {
                Id = 2;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 1.5; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            };
            {
                Id = 3;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 2.0; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            };
            {
                Id = 4;
                OwnerId = 0;
                Circle = 
                    {
                        Position = { X = 0.5; Y = 0.0; };
                        Radius = 0.5;
                    };
                Health = 250;
            }
        |]
    let heatMapTurn = { Turn = 1; Entities = entities; }

    let resultEntities = entitiesCollidingAndSelf heatMapTurn entities.[0] 0.0

    Assert.Equal(4, resultEntities.Length)

[<Fact>]
let ``Calculate number of turns should return 0`` () =
    Assert.Equal(0, calculateNumberOfTurns 0)

[<Fact>]
let ``Calculate number of turns should return 1 if hit five`` () =
    Assert.Equal(1, calculateNumberOfTurns 5)

[<Fact>]
let ``Calculate number of turns should return 1 if hit seven`` () =
    Assert.Equal(1, calculateNumberOfTurns 7)

[<Fact>]
let ``Calculate number of turns should return 2 if hit eight`` () =
    Assert.Equal(2, calculateNumberOfTurns 8)

[<Fact>]
let ``Calculate number of turns should return 2 if hit fourteen`` () =
    Assert.Equal(2, calculateNumberOfTurns 14)