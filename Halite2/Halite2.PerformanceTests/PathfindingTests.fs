module PathfindingTests

open ExecutePerformanceTests
open Halite
open Pathfinding

let executeAll times =
    logPerfs 
        "calculateNumberOfTurns" 
        (fun _ -> (calculateNumberOfTurns 15) |> ignore) 
        times

    let planet1 = {
        Entity = 
            {
                Id = 1;
                OwnerId = 0;
                Circle = 
                    {
                        Position = 
                            {
                                X = 10.0;
                                Y = 10.0;
                            };
                        Radius = 5.5;
                    };
                Health = 255;
            };
        NumDockingSpots = 4;
        NumDockedShips = 3;
        CurrentProduction = 6;
        RemainingResources = 20;
        DockedShipIds = [||];
        Owned = 0;
    }

    let planets = [| planet1 |]

    let myShips = [||]

    logPerfs 
        "createHeatMap" 
        (fun _ -> (createHeatMap 10 planets myShips) |> ignore) 
        times