module Pathfinding
open Collisions
open Halite

type HeatMap = {
    Entities: Entity[];
}

let createHeatMap (planets: Planet[]) (myShips: Ship[]) = 
    let planetEntities = 
        planets
        |> Array.map (fun p -> p.Entity)

    let myShipEntities = 
        myShips
        |> Array.map (fun p -> p.Entity)

    {      
        Entities = Array.append planetEntities myShipEntities
    }

let navigateToPlanet heatMap ship planet =
    ""