module Pathfinding
open Collisions
open Halite

type HeatMap = {
    Width: int;
    Height: int;
    Circles: Circle[];
}

let createHeatMap gameMap myPlayer = 
    let planetCircles = 
        gameMap.Planets
        |> Array.map (fun p -> p.Entity.Circle)

    let stationaryShipCircles = 
        myPlayer.Ships
        |> Array.filter (fun s -> s.DockingStatus <> Undocked)
        |> Array.map (fun p -> p.Entity.Circle)

    {
        Width = gameMap.Width;  
        Height = gameMap.Height;      
        Circles = Array.append planetCircles stationaryShipCircles
    }