module Pathfinding

open Constants
open Collisions
open Halite
open Logs

type HeatMap = {
    Entities: Entity[];
}

type MoveOrder = {
    Speed: int;
    Angle: int;
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

let calculateNewPosition (position: Position) distance angle =
    let radAngle = degreesToRadians (float angle)
    let x = position.X + distance * cos radAngle
    let y = position.Y + distance * sin radAngle
    { X = x; Y = y }

let anglesToCheckForPlanetMining = 
    [0..180]
    |> List.map 
        (fun i -> 
            if (i = 0 || i = 180)
            then [ i ]
            else [ i; -i; ]                
        )
    |> List.reduce List.append

let tryGetPlanetDockingDestination heatMap (ship: Ship) (planet: Planet) =
    let baseAngle = round (calculateAngleTo planet.Entity.Circle.Position ship.Entity.Circle.Position) |> int
    let distanceToPlanet = (planet.Entity.Circle.Radius + 2.0)

    let circleOfSolution = 
        {
            Position = planet.Entity.Circle.Position;
            Radius = distanceToPlanet;
        }

    let filteredHeatMap =
        heatMap.Entities
        |> Array.filter 
            (fun entity -> 
                entity.Id <> ship.Entity.Id &&
                circlesCollide circleOfSolution entity.Circle
            )

    anglesToCheckForPlanetMining
    |> List.tryFind 
        (fun angle ->
            let futurePosition = calculateNewPosition planet.Entity.Circle.Position distanceToPlanet (baseAngle + angle)

            filteredHeatMap
            |> Array.exists 
                (fun e -> 
                    circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                )
            |> not
        )
    |> (fun angleOption ->
            match angleOption with
            | Some angle -> Some (calculateNewPosition planet.Entity.Circle.Position distanceToPlanet (baseAngle + angle))
            | None -> None
        )     

let canThrust heatMap position speed angle = 
    true // TODO

let tryGoForward heatMap (from: Position) (dest: Position) =
    let distanceToDest = floor (calculateDistanceTo from dest)
    let distanceToDestInt = distanceToDest |> int
    let angle = round (calculateAngleTo from dest) |> int

    match canThrust heatMap from distanceToDest angle with
        | false -> None
        | true ->
        (
            let numberOfTurns = ceil( distanceToDest / (float MAX_SPEED) ) |> int       
            if numberOfTurns = 0 
            then Some []
            else
            Some
                ( 
                    [ for turn in 1 .. numberOfTurns -> 
                        (
                            if (turn = numberOfTurns && (distanceToDestInt % MAX_SPEED) <> 0) 
                            then distanceToDestInt % MAX_SPEED
                            else 7
                        )
                    ]
                    |> List.map (fun s -> { Speed = s; Angle = angle; }) 
                )
        )  

let tryChooseBestPath heatMap (from: Position) (dest: Position) =
    match tryGoForward heatMap from dest with
        | Some path -> Some path       
        | None ->
        (
            // TODO : find best path with recursive (tangent of obstacles)            
            None
        )

let navigateToPlanet heatMap (ship: Ship) (planet: Planet) =
    // target one destination point (close to planet)
    let destinationOption = tryGetPlanetDockingDestination heatMap ship planet

    match destinationOption with
        | None -> ""
        | Some dest -> 
        (
            // choose best move orders (path) to go to dest
            let bestPathOption = tryChooseBestPath heatMap ship.Entity.Circle.Position dest
        
            match bestPathOption with
                | None -> ""
                | Some [] -> ""
                | Some bestPath ->
                (
                    // TODO : update heatmap based on orders

                    // move using the first order
                    let firstOrder = bestPath.[0]
                    thrust ship firstOrder.Speed firstOrder.Angle
                )
        )