﻿module Groups

open System
open Halite
open Statistics
open Collisions

type GroupMission = 
    | Mining
    | Attack

type Target =
    | Planet of Planet
    | Ship of Ship

type Group = {
    Ship: Ship; // TODO : From 1 ship per group To N ships per group
    Mission: GroupMission option;
    Target: Target option;
}

let getLivingGroups (planets: Planet[]) (enemyShips: Ship[]) (myShips: Ship[]) groups = 
    groups 
    |> Array.map (fun g -> (g, myShips |> Array.tryFind (fun s -> s.Entity.Id = g.Ship.Entity.Id)))
    |> Array.filter (fun (_, shipOption) -> shipOption.IsSome)
    |> Array.map 
        (fun (g, shipOption) -> 
            { g with 
                Target = 
                    match g.Target with
                    | None -> None
                    | Some (Planet t) -> (planets |> Array.tryFind (fun p -> p.Entity.Id = t.Entity.Id)) |> (fun p -> if p.IsNone then None else Some (Planet p.Value))
                    | Some (Ship t) -> (enemyShips |> Array.tryFind (fun s -> s.Entity.Id = t.Entity.Id)) |> (fun s -> if s.IsNone then None else Some (Ship s.Value))
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

let dockableShips (planet: Planet) (myShips: Ship[]) =
    myShips
    |> Array.filter (fun s -> canDock s planet)

type MiningPlanet = {
    Planet: Planet;
    SlotsToMine: int;
}

let tryGetPlanetTarget (target: Target option) =
    match target with
    | None -> None
    | Some (Planet p) -> Some p
    | _ -> None

let orderNewGroupsSmartMining (existingGroups: Group[]) (planetsStatsToMine: PlanetStat[]) (myShips: Ship[]) =
    let slotsToMine = 
        planetsStatsToMine
        |> Array.map 
            (fun stat -> 
                let currentlyDockingShipsToPlanet =
                    existingGroups
                    |> Array.filter 
                        (fun g -> 
                            let planetTarget = tryGetPlanetTarget g.Target

                            g.Ship.DockingStatus = Docking &&
                            planetTarget.IsSome &&
                            planetTarget.Value.Entity.Id = stat.Planet.Entity.Id &&
                            g.Mission.IsSome && 
                            g.Mission.Value = Mining
                        )

                { 
                    Planet = stat.Planet;
                    SlotsToMine = stat.SlotsAvailable - (currentlyDockingShipsToPlanet.Length); 
                }
            )
        |> Array.filter (fun stm -> stm.SlotsToMine > 0)  

    if (slotsToMine.Length <= 0)
    then [||]
    else
        let mutable myUndockedShips = myShips |> Array.filter (fun s -> s.DockingStatus = Undocked)

        let groupsOfImmediateDockableShips =
            slotsToMine
            |> Array.map 
                (fun stm ->
                    let groups = 
                        dockableShips stm.Planet myUndockedShips
                        |> Array.truncate stm.SlotsToMine
                        |> Array.map 
                            (fun s -> 
                                { 
                                    Ship = s; 
                                    Mission = Some Mining; 
                                    Target = Some (Planet stm.Planet);
                                }
                            )

                    myUndockedShips <- getUnassignedShips groups myUndockedShips

                    groups
                )
            |> Array.reduce Array.append

        let remainingSlotsToMine = 
            slotsToMine
            |> Array.map 
                (fun stm ->
                    let numberOfShipsImmediatelyDocked = 
                        groupsOfImmediateDockableShips
                        |> Array.filter 
                            (fun g -> 
                                let planetTarget = tryGetPlanetTarget g.Target

                                planetTarget.IsSome && 
                                planetTarget.Value.Entity.Id = stm.Planet.Entity.Id
                            )
                        |> Array.length

                    { stm with SlotsToMine = stm.SlotsToMine - numberOfShipsImmediatelyDocked }
                )
            |> Array.filter (fun stm -> stm.SlotsToMine > 0)        

        if (remainingSlotsToMine.Length <= 0)
        then groupsOfImmediateDockableShips
        else
            let remainingShips = getUnassignedShips groupsOfImmediateDockableShips myUndockedShips

            let maxSlotToMine = remainingSlotsToMine |> Array.map (fun stm -> stm.SlotsToMine) |> Array.max

            let otherGroups = 
                [| 1..maxSlotToMine |]
                |> Array.map 
                    (fun i -> 
                        remainingSlotsToMine
                        |> Array.filter (fun stm -> stm.SlotsToMine >= i)
                        |> Array.map (fun stm -> stm.Planet)
                    )
                |> Array.reduce Array.append
                |> Array.truncate remainingShips.Length
                |> Array.indexed
                |> Array.map 
                    (fun (i, p) -> 
                        { 
                            Ship = remainingShips.[i]; 
                            Mission = Some Mining; 
                            Target = Some (Planet p);
                        }
                    )

            Array.append groupsOfImmediateDockableShips otherGroups

let orderNewGroupsFullMining (planetsToConquer: Planet[]) (myShips: Ship[]) =
    let numberOfPlanetsToConquer = planetsToConquer.Length

    myShips
    |> Array.indexed
    |> Array.map 
        (fun (i, s) -> 
            { 
                Ship = s; 
                Mission = Some Mining; 
                Target = Some (Planet planetsToConquer.[i % numberOfPlanetsToConquer]);
            }
        )

let orderNewGroupsSimpleMining (planetsToConquer: Planet[]) (myShips: Ship[]) =
    myShips
    |> Array.take (min myShips.Length planetsToConquer.Length)
    |> Array.indexed
    |> Array.map (fun (i, s) -> { Ship = s; Mission = Some Mining; Target = Some (Planet planetsToConquer.[i]); })

let orderNewGroupsDumbAttack (enemyShips: Ship[]) (myShips: Ship[]) =
    let dockedEnemyShips =
        enemyShips
        |> Array.filter (fun s -> s.DockingStatus = Docked)

    myShips
    |> Array.map 
        (fun myShip ->
            // find closest enemy ship
            let enemy = 
                dockedEnemyShips
                |> Array.sortBy (fun es -> calculateDistanceTo myShip.Entity.Circle.Position es.Entity.Circle.Position)
                |> Array.tryHead

            {
                Ship = myShip;
                Mission = Some Attack;
                Target = 
                    match enemy with
                    | None -> None
                    | Some e -> Some (Ship e)
            }
        )