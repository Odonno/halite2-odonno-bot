module CollisionsTests

open ExecutePerformanceTests
open Collisions

let executeAll times =
    let p1 = { X = 1.5; Y = 1.5; }
    let p2 = { X = 2.3; Y = 8.4; }

    logPerfs 
        "calculateDistanceTo" 
        (fun _ -> (calculateDistanceTo p1 p2) |> ignore) 
        times

    let angle = 50.0
    
    logPerfs
        "degreesToRadians"
        (fun _ -> (degreesToRadians angle) |> ignore) 
        times
        
    logPerfs
        "radiansToDegrees"
        (fun _ -> (radiansToDegrees angle) |> ignore) 
        times
    
    logPerfs 
        "calculateRadAngleTo" 
        (fun _ -> (calculateRadAngleTo p1 p2) |> ignore) 
        times

    logPerfs 
        "calculateAngleTo" 
        (fun _ -> (calculateAngleTo p1 p2) |> ignore) 
        times

    let c1 = { Position = p1; Radius = 3.7; }
    let c2 = { Position = p2; Radius = 4.3; }

    logPerfs 
        "circlesCollide" 
        (fun _ -> (circlesCollide c1 c2) |> ignore) 
        times
        
    logPerfs 
        "isInCircle" 
        (fun _ -> (isInCircle p1 c1) |> ignore) 
        times
        