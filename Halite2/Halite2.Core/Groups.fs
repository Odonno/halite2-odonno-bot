module Groups

open System
open Halite
open Statistics
open Collisions
open Constants

type GroupMission = 
    | Mining
    | Attack
    | Protect

type Target =
    | Planet of Planet
    | Ship of Ship

type Group = {
    Ship: Ship; // TODO : From 1 ship per group To N ships per group
    Mission: GroupMission option;
    Target: Target option;
    DistanceToTarget: float option;
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

let unassignMiningGroupIfNoInterest (planetsStatsToMine: PlanetStat[]) groups =
    let planetsToMine = planetsStatsToMine |> Array.map (fun stat -> stat.Planet)

    groups 
    |> Array.filter 
        (fun g ->
            let toUnassign = 
                g.Mission.IsSome && 
                g.Mission.Value = Mining &&
                g.Ship.DockingStatus = Undocked &&
                g.Target.IsSome &&
                (
                    planetsToMine 
                    |> Array.exists 
                        (fun p -> 
                            match g.Target with
                            | Some (Planet planet) -> p.Entity.Id = planet.Entity.Id
                            | _ -> false
                        ) 
                    |> not
                )

            not toUnassign
        )

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
    Risk: float;
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
                    Risk = stat.Risk;
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
                                    DistanceToTarget = 
                                        Some (calculateDistanceTo s.Entity.Circle.Position stm.Planet.Entity.Circle.Position);
                                }
                            )

                    myUndockedShips <- getUnassignedShips groups myUndockedShips

                    groups
                )
            |> Array.reduce Array.append

        let remainingMiningPlanets = 
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

        if (remainingMiningPlanets.Length <= 0)
        then groupsOfImmediateDockableShips
        else
            let mutable remainingShips = getUnassignedShips groupsOfImmediateDockableShips myUndockedShips

            let remainingMiningPlanetsWithoutRisk =
                remainingMiningPlanets
                |> Array.filter (fun stm -> stm.Risk = 1.0)

            if (remainingMiningPlanetsWithoutRisk.Length <= 0)
            then 
                let remainingMiningPlanetsWithRisk =
                    remainingMiningPlanets 
                    |> Array.filter (fun stm -> stm.Risk <> 1.0)

                let maxSlotToMine = 
                    remainingMiningPlanetsWithRisk
                    |> Array.map (fun stm -> stm.SlotsToMine) 
                    |> Array.max

                let otherGroups = 
                    [| 1..maxSlotToMine |]
                    |> Array.map 
                        (fun i -> 
                            remainingMiningPlanets
                            |> Array.filter (fun stm -> stm.SlotsToMine >= i)
                            |> Array.map (fun stm -> stm.Planet)
                        )
                    |> Array.reduce Array.append
                    |> Array.truncate remainingShips.Length
                    |> Array.indexed
                    |> Array.map 
                        (fun (i, p) -> 
                            let ship = remainingShips.[i]

                            { 
                                Ship = ship; 
                                Mission = Some Mining; 
                                Target = Some (Planet p);
                                DistanceToTarget = 
                                    Some (calculateDistanceTo ship.Entity.Circle.Position p.Entity.Circle.Position);
                            }
                        )

                Array.concat [ groupsOfImmediateDockableShips; otherGroups; ]
            else
                let mutable slotsWithoutRisk = 
                    remainingMiningPlanetsWithoutRisk 
                    |> Array.map (fun stm -> List.init stm.SlotsToMine (fun i -> (i, stm)))
                    |> Array.reduce List.append

                let groupsWithoutRisk =
                    [| 0..(slotsWithoutRisk.Length - 1) |]
                    |> Array.choose 
                        (fun _ ->
                            if slotsWithoutRisk.Length <= 0 || remainingShips.Length <= 0
                            then None
                            else
                                let distinctSlots = 
                                    slotsWithoutRisk 
                                    |> List.distinctBy (fun (_, swr) -> swr.Planet.Entity.Id) 

                                let (ship, (j, stm)) =
                                    remainingShips
                                    |> Array.map 
                                        (fun s ->
                                            distinctSlots
                                            |> List.map (fun slot -> (s, slot))
                                        )
                                    |> Array.reduce List.append
                                    |> List.minBy (fun (ship, (_, stm)) -> calculateDistanceTo ship.Entity.Circle.Position stm.Planet.Entity.Circle.Position)

                                let group = 
                                    {
                                        Ship = ship; 
                                        Mission = Some Mining; 
                                        Target = Some (Planet stm.Planet);
                                        DistanceToTarget = 
                                            Some (calculateDistanceTo ship.Entity.Circle.Position stm.Planet.Entity.Circle.Position);
                                    }

                                remainingShips <- getUnassignedShips [| group |] remainingShips
                                slotsWithoutRisk <-
                                    slotsWithoutRisk
                                    |> List.filter (fun (k, swr) -> j <> k && stm.Planet.Entity.Id <> swr.Planet.Entity.Id)

                                Some group
                        )

                let remainingMiningPlanetsWithRisk =
                    remainingMiningPlanets 
                    |> Array.filter (fun stm -> stm.Risk <> 1.0)

                if (remainingMiningPlanetsWithRisk.Length <= 0)
                then Array.concat [ groupsOfImmediateDockableShips; groupsWithoutRisk; ]
                else
                    let maxSlotToMine = 
                        remainingMiningPlanetsWithRisk
                        |> Array.map (fun stm -> stm.SlotsToMine) 
                        |> Array.max

                    let otherGroups = 
                        [| 1..maxSlotToMine |]
                        |> Array.map 
                            (fun i -> 
                                remainingMiningPlanetsWithRisk
                                |> Array.filter (fun stm -> stm.SlotsToMine >= i)
                                |> Array.map (fun stm -> stm.Planet)
                            )
                        |> Array.reduce Array.append
                        |> Array.truncate remainingShips.Length
                        |> Array.indexed
                        |> Array.map 
                            (fun (i, p) -> 
                                let ship = remainingShips.[i]

                                { 
                                    Ship = ship; 
                                    Mission = Some Mining; 
                                    Target = Some (Planet p);
                                    DistanceToTarget = 
                                        Some (calculateDistanceTo ship.Entity.Circle.Position p.Entity.Circle.Position);
                                }
                            )

                    Array.concat [ groupsOfImmediateDockableShips; groupsWithoutRisk; otherGroups; ]

let orderNewGroupsFullMining (planetsToConquer: Planet[]) (myShips: Ship[]) =
    let numberOfPlanetsToConquer = planetsToConquer.Length

    myShips
    |> Array.indexed
    |> Array.map 
        (fun (i, s) -> 
            let planet = planetsToConquer.[i % numberOfPlanetsToConquer]

            { 
                Ship = s; 
                Mission = Some Mining; 
                Target = Some (Planet planet);
                DistanceToTarget = 
                    Some (calculateDistanceTo s.Entity.Circle.Position planet.Entity.Circle.Position);
            }
        )

let orderNewGroupsSimpleMining (planetsToConquer: Planet[]) (myShips: Ship[]) =
    myShips
    |> Array.take (min myShips.Length planetsToConquer.Length)
    |> Array.indexed
    |> Array.map 
        (fun (i, s) -> 
            let planet = planetsToConquer.[i]

            { 
                Ship = s; 
                Mission = Some Mining; 
                Target = Some (Planet planet);
                DistanceToTarget = 
                    Some (calculateDistanceTo s.Entity.Circle.Position planet.Entity.Circle.Position);
            }
        )

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
                DistanceToTarget = 
                    match enemy with
                    | None -> None
                    | Some e -> Some (calculateDistanceTo myShip.Entity.Circle.Position e.Entity.Circle.Position)
            }
        )

let orderNewGroupsDumbProtect (existingGroups: Group[]) (planetsStatsToMine: PlanetStat[]) (enemyShips: Ship[]) (myShips: Ship[]) =   
    let groupsToProtect =
        existingGroups
        |> Array.filter 
            (fun g -> 
                let distanceToClosestEnemy = 
                    enemyShips
                    |> Array.map (fun enemyShip -> calculateDistanceTo enemyShip.Entity.Circle.Position g.Ship.Entity.Circle.Position)
                    |> Array.min                

                match tryGetPlanetTarget g.Target with
                | None -> false
                | Some targetPlanet ->
                (
                    let planetStatOption = 
                        planetsStatsToMine 
                        |> Array.filter (fun stat -> stat.Planet.Entity.Id = targetPlanet.Entity.Id)
                        |> Array.tryHead

                    match planetStatOption with
                    | None -> false
                    | Some planetStat ->
                    (
                        g.Ship.DockingStatus <> Undocked &&
                        g.Target.IsSome &&
                        g.Mission.IsSome && 
                        g.Mission.Value = Mining &&
                        distanceToClosestEnemy < float (MAX_SPEED * (planetStat.NumberOfTurnsBeforeNewShip + 1))
                    )
                )
            )

    if groupsToProtect.Length <= 0
    then [||]
    else
        let mutable myUndockedShips = myShips |> Array.filter (fun s -> s.DockingStatus = Undocked)

        groupsToProtect
        |> Array.truncate myUndockedShips.Length
        |> Array.map 
            (fun groupToProtect ->
                let closestAllyShip =   
                    myUndockedShips
                    |> Array.sortBy (fun s -> calculateDistanceTo s.Entity.Circle.Position groupToProtect.Ship.Entity.Circle.Position)              
                    |> Array.head

                let protectGroup =
                    {
                        Ship = closestAllyShip;
                        Mission = Some Protect;
                        Target = Some (Ship groupToProtect.Ship);
                        DistanceToTarget = 
                            Some (calculateDistanceTo closestAllyShip.Entity.Circle.Position groupToProtect.Ship.Entity.Circle.Position);
                    }

                myUndockedShips <- getUnassignedShips [| protectGroup |] myUndockedShips

                protectGroup
            )