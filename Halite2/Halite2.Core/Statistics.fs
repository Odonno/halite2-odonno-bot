module Statistics

open System
open Constants
open Halite
open Collisions
open Logs

type PlanetStat = {
    Planet: Planet;
    MinimumReachDistance: float; // 0 to N (lowest is better)
    Risk: float; // -1 to 1 (highest is better/safer)
    ProductionInterest: float; // 0 to 1 (highest is better)
    IsAlly: bool;
    IsEnemy: bool;
    IsNeutral: bool;
    SlotsAvailable: int;
    CanProduceMore: bool;
    NumberOfTurnsBeforeNewShip: int;
}

let analysePlanet planet player (undockedEnemyShips: Ship[]) biggestPlanet smallestPlanet =
    let productionInterest = 
        (planet.Entity.Circle.Radius - smallestPlanet.Entity.Circle.Radius) / 
        (biggestPlanet.Entity.Circle.Radius - smallestPlanet.Entity.Circle.Radius);

    let minimumReachDistanceOption = 
        player.Ships
        |> Array.filter (fun s -> s.DockingStatus = DockingStatus.Undocked)
        |> Array.map (fun s -> calculateDistanceTo planet.Entity.Circle.Position s.Entity.Circle.Position)
        |> Array.sort
        |> Array.tryHead

    let minimumReachDistance = 
        match minimumReachDistanceOption with
            | Some x -> x
            | _ -> 500.0

    let isNeutral = planet.Owned = 0
    let isAlly = planet.Owned <> 0 && planet.Entity.OwnerId = player.Id
    let isEnemy = planet.Owned <> 0 && planet.Entity.OwnerId <> player.Id
    
    let risk = 
        match isNeutral with
        | true -> 
            (
                undockedEnemyShips
                |> Array.map (fun s -> (minimumReachDistance + (2.0 * float MAX_SPEED)) - (calculateDistanceTo planet.Entity.Circle.Position s.Entity.Circle.Position))
                |> Array.sortDescending
                |> Array.take (min undockedEnemyShips.Length 5)
                |> Array.sumBy (fun x -> Math.Clamp(x * -1.0 * 0.2, -0.2, 0.2))
            )
        | _ -> 
            (
                match isAlly with
                | true -> 1.0
                | _ -> -1.0
            )

    let slotsAvailable = planet.NumDockingSpots - planet.NumDockedShips
    let unitsLeftToProduceNewShip = PRODUCTIVITY_REQUIRED_FOR_NEW_SHIP - planet.CurrentProduction
    let numberOfTurnsToProduceNewShip = 
        (float unitsLeftToProduceNewShip / (float BASE_PRODUCTIVITY * float planet.NumDockedShips)) |> ceil |> int

    {
        Planet = planet;
        MinimumReachDistance = minimumReachDistance;
        Risk = risk;
        ProductionInterest = productionInterest;
        IsAlly = isAlly;
        IsEnemy = isEnemy;
        IsNeutral = isNeutral;
        SlotsAvailable = slotsAvailable;
        CanProduceMore = slotsAvailable > 0;
        NumberOfTurnsBeforeNewShip = numberOfTurnsToProduceNewShip;
    }