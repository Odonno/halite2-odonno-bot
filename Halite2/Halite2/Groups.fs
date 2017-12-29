module Groups

open Halite
open System

type GroupMission = 
    | Mining
    | Attack

type Group = {
    Ship: Ship; // TODO : From 1 ship per group To N ships per group
    Mission: GroupMission option;
    Target: Planet option;
}

let getLivingGroups (planets: Planet[]) (myShips: Ship[]) groups = 
    groups 
    |> Array.map (fun g -> (g, myShips |> Array.tryFind (fun s -> s.Entity.Id = g.Ship.Entity.Id)))
    |> Array.filter (fun (_, shipOption) -> shipOption.IsSome)
    |> Array.map 
        (fun (g, shipOption) -> 
            { g with 
                Target = 
                    match g.Target with
                    | None -> None
                    | Some t -> (planets |> Array.tryFind (fun p -> p.Entity.Id = t.Entity.Id));
                Ship = shipOption.Value
            }
        )

let getGroupsWithTarget groups = 
    groups |> Array.filter (fun g -> g.Target.IsSome)

let getGroupsWithUndockedShip groups = 
    groups |> Array.filter (fun g -> g.Ship.DockingStatus = Undocked)

let getUnassignedShips groups (myShips: Ship[]) =
    myShips 
    |> Array.filter 
        (fun s -> 
            groups 
            |> Array.exists (fun g -> g.Ship.Entity.Id = s.Entity.Id) |> not
        )

let orderNewGroupsFull (planetsToConquer: Planet[]) (myShips: Ship[]) =
    let numberOfPlanetsToConquer = planetsToConquer.Length

    myShips
    |> Array.indexed
    |> Array.map 
        (fun (i, s) -> 
            { 
                Ship = s; 
                Mission = Some Mining; 
                Target = Some planetsToConquer.[i % numberOfPlanetsToConquer]; 
            }
        )

let orderNewGroupsSimple (planetsToConquer: Planet[]) (myShips: Ship[]) =
    myShips
    |> Array.take (min myShips.Length planetsToConquer.Length)
    |> Array.indexed
    |> Array.map (fun (i, s) -> { Ship = s; Mission = Some Mining; Target = Some planetsToConquer.[i]; })