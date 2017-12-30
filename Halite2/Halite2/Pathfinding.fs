module Pathfinding

open Constants
open Collisions
open Halite
open Logs
open System

type HeatMap = {
    Entities: Entity[];
}

type MoveOrder = {
    Speed: int;
    Angle: int;
}

type PathSide =
    | Left
    | Right

type PathOrObstacle =
    | Path of MoveOrder list
    | Obstacle of Entity   

let calculateNewPosition (position: Position) distance angle =
    let radAngle = degreesToRadians (float angle)
    let x = position.X + distance * cos radAngle
    let y = position.Y + distance * sin radAngle
    { X = x; Y = y }

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

let updateHeatMapWithNewPath heatMap (ship: Ship) path =
    let mutable futurePosition = ship.Entity.Circle.Position

    let moveEntities =
        path
        |> List.map 
            (fun moveOrder ->
                [| 1..(moveOrder.Speed) |]
                |> Array.map 
                    (fun _ ->
                        futurePosition <- calculateNewPosition futurePosition 1.0 moveOrder.Angle
                        let circle = { Position = futurePosition; Radius = SHIP_RADIUS; }
                        { ship.Entity with Circle = circle; }
                    )
            )
        |> List.reduce Array.append
        
    {
        Entities = Array.append heatMap.Entities moveEntities
    }

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
        |> Array.filter (fun entity -> circlesCollide circleOfSolution entity.Circle)

    anglesToCheckForPlanetMining
    |> List.tryPick 
        (fun angle ->
            let futurePosition = calculateNewPosition planet.Entity.Circle.Position distanceToPlanet (baseAngle + angle)

            let hasObstacles = 
                filteredHeatMap
                |> Array.exists 
                    (fun e -> 
                        circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                    )
            
            if hasObstacles
            then None
            else Some futurePosition
        )

let canThrust heatMap (from: Position) speed angle = 
    [1.0..(float speed)]
    |> Seq.forall 
        (fun s ->
            let futurePosition = calculateNewPosition from s angle

            heatMap.Entities
            |> Array.exists 
                (fun e -> 
                    circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                )
            |> not
        )

let tryFindObstacle heatMap (from: Position) speed angle = 
    [1.0..(float speed)]
    |> Seq.tryPick 
        (fun s ->
            let futurePosition = calculateNewPosition from s angle

            heatMap.Entities
            |> Array.tryFind 
                (fun e -> 
                    circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                )           
        )

let goForwardOrReturnFirstObstacle heatMap (from: Position) (dest: Position) =
    let distanceToDest = floor (calculateDistanceTo from dest)
    let distanceToDestInt = distanceToDest |> int
    let angle = round (calculateAngleTo from dest) |> int

    match tryFindObstacle heatMap from distanceToDestInt angle with
        | Some obstacle -> obstacle |> Obstacle
        | None ->
        (
            let numberOfTurns = ceil( distanceToDest / (float MAX_SPEED) ) |> int       
            if numberOfTurns = 0 
            then [] |> Path
            else
                ( 
                    [ for turn in 1 .. numberOfTurns -> 
                        (
                            if (turn = numberOfTurns && (distanceToDestInt % MAX_SPEED) <> 0) 
                            then distanceToDestInt % MAX_SPEED
                            else 7
                        )
                    ]
                    |> List.map (fun s -> { Speed = s; Angle = angle; }) 
                ) |> Path
        )  

let tryGetPositionBetween heatMap from minSpeed maxSpeed angle =
    [1..maxSpeed]
    |> Seq.tryPick 
        (fun s ->
            let futurePosition = calculateNewPosition from (float s) angle
            
            let hasObstacles = 
                heatMap.Entities
                |> Array.exists 
                    (fun e -> 
                        circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                    )
            
            if hasObstacles
            then None
            else Some (s, futurePosition)              
        )
    |> 
        (fun x ->
            match x with
                | None -> None
                | Some (s, _) when s < minSpeed -> None
                | Some (_, position) -> Some position
        )    

let tryChooseBestPath heatMap (from: Position) (dest: Position) =
    let rec tryChooseBestPath heatMap (from: Position) (dest: Position) pathSideOption (previousObstacleOption: Entity option) =
        match goForwardOrReturnFirstObstacle heatMap from dest with
            | Path path -> Some path       
            | Obstacle obstacle ->
            (
                let useLeftPath = 
                    match (pathSideOption, previousObstacleOption) with
                    | (Some pathSide, Some previousObstacle) 
                        when (pathSide = Right && previousObstacle.Id = obstacle.Id) -> false
                    | _ -> true
                let useRightPath = 
                    match (pathSideOption, previousObstacleOption) with
                    | (Some pathSide, Some previousObstacle) 
                        when (pathSide = Left && previousObstacle.Id = obstacle.Id) -> false
                    | _ -> true

                // find best path with recursive (tangent of obstacles)   
                let tangents = circleTangentsFromPoint from obstacle.Circle
                
                // find best path from left or right tangent
                let leftTangent = tangents.[0]
                let rightTangent = tangents.[1]

                // calculate angles of the tangents (based on the from location)
                let distanceFromToDest = calculateDistanceTo from dest
                let baseAngle = calculateAngleTo from dest

                let tangentAngle = (calculateAngleTo dest leftTangent) |> floor

                let leftAngle = (baseAngle - tangentAngle) |> floor
                let rightAngle = (baseAngle + tangentAngle) |> floor

                // get min length ("from" to "tangent" point length)
                let minSpeedTangent = ceil(calculateDistanceTo from leftTangent) |> int

                // get max length of the tangent (right angle to dest)
                let maxSpeedTangent = ceil(distanceFromToDest * cos leftAngle) |> int

                // left tangent
                let leftPath = 
                    match useLeftPath with
                    | false -> None
                    | true ->
                    (
                        let leftPositionBeforeObstacleOption = tryGetPositionBetween heatMap from minSpeedTangent maxSpeedTangent (int leftAngle)
                        match leftPositionBeforeObstacleOption with
                        | None -> None
                        | Some leftPosition -> tryChooseBestPath heatMap leftPosition dest (Some Left) (Some obstacle)
                    )

                // right tangent
                let rightPath = 
                    match useRightPath with
                    | false -> None
                    | true ->
                    (
                        let rightPositionBeforeObstacle = tryGetPositionBetween heatMap from minSpeedTangent maxSpeedTangent (int rightAngle)
                        match rightPositionBeforeObstacle with
                        | None -> None
                        | Some rightPosition -> tryChooseBestPath heatMap rightPosition dest (Some Right) (Some obstacle)
                    )

                // use the fastest path
                match (leftPath, rightPath) with
                    | (None, None) -> None
                    | (Some left, None) -> Some left
                    | (None, Some right) -> Some right
                    | (Some left, Some right) -> 
                        Some (
                            if left.Length <= right.Length 
                            then left 
                            else right
                        )
            )
    tryChooseBestPath heatMap from dest None None

let navigateToPlanet heatMap (ship: Ship) (planet: Planet) =
    // use heat map without own ship
    let heatMapWithoutOwnShip =
        { Entities = heatMap.Entities |> Array.filter (fun e -> e.Id <> ship.Entity.Id) }

    // target one destination point (close to planet)
    let destinationOption = tryGetPlanetDockingDestination heatMapWithoutOwnShip ship planet

    match destinationOption with
        | None -> (heatMap, "")
        | Some dest -> 
        (
            // choose best move orders (path) to go to dest
            let bestPathOption = tryChooseBestPath heatMapWithoutOwnShip ship.Entity.Circle.Position dest
        
            match bestPathOption with
                | None -> (heatMap, "")
                | Some [] -> (heatMap, "")
                | Some bestPath ->
                (
                    // update heatmap based on orders
                    let newHeatMap = updateHeatMapWithNewPath heatMap ship bestPath

                    // move using the first order
                    let firstOrder = bestPath.[0]
                    let command = thrust ship firstOrder.Speed firstOrder.Angle

                    (newHeatMap, command)
                )
        )