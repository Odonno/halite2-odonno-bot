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

type Path = MoveOrder list

type PathSide =
    | Left
    | Right

type PathOrObstacle =
    | Path of Path
    | Obstacle of Entity   

let calculateNewPosition (position: Position) distance angle =
    let radAngle = degreesToRadians (float angle)
    let x = position.X + distance * cos radAngle
    let y = position.Y + distance * sin radAngle
    { X = x; Y = y }

let entitiesCollidingAndSelf heatMap entity expandedRadius =
    let rec recursiveEntitiesCollidingAndSelf entityToCompare entitiesToMatch entitiesFound =
        match entitiesToMatch with
        | [] -> entitiesFound
        | head :: tail -> 
        (
            if circlesCollideWithExpandedRadius head.Circle entityToCompare.Circle expandedRadius
            then 
                recursiveEntitiesCollidingAndSelf 
                    entityToCompare 
                    tail
                    (head::entitiesFound)
                @
                recursiveEntitiesCollidingAndSelf 
                    head 
                    tail
                    []
                |> List.distinctBy (fun e -> e.Id)
            else
                recursiveEntitiesCollidingAndSelf entityToCompare tail entitiesFound
        )        

    recursiveEntitiesCollidingAndSelf entity (heatMap.Entities |> Array.toList) []

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

let anglesToCheckForDestination = 
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

    anglesToCheckForDestination
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

let tryGetPositionNearEnemyShip heatMap (ship: Ship) (enemyShip: Ship) =
    let baseAngle = 
        match enemyShip.DockingStatus with
        | Undocked -> round (calculateAngleTo enemyShip.Entity.Circle.Position ship.Entity.Circle.Position) |> int
        | _ -> 
        (
            let planetEntity = heatMap.Entities |> Array.find (fun e -> e.Id = enemyShip.PlanetId)
            round (calculateAngleTo planetEntity.Circle.Position enemyShip.Entity.Circle.Position) |> int
        )        

    let distanceToEnemyShip = (enemyShip.Entity.Circle.Radius + 3.0)

    let circleOfSolution = 
        {
            Position = enemyShip.Entity.Circle.Position;
            Radius = distanceToEnemyShip;
        }

    let filteredHeatMap =
        heatMap.Entities
        |> Array.filter (fun entity -> circlesCollide circleOfSolution entity.Circle)
        
    anglesToCheckForDestination
    |> List.tryPick 
        (fun angle ->
            let futurePosition = calculateNewPosition enemyShip.Entity.Circle.Position distanceToEnemyShip (baseAngle + angle)

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

let canFight (ship: Ship) (enemyShip: Ship) =
    let distance = calculateDistanceTo ship.Entity.Circle.Position enemyShip.Entity.Circle.Position
    distance <= WEAPON_RADIUS

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

let goForwardOrReturnFirstObstacle heatMap (from: Position) (target: Position) (minRadius: float) (maxRadius: float) =
    let distanceToTarget = calculateDistanceTo from target

    let minDistanceToTarget = ceil(distanceToTarget - maxRadius)
    let maxDistanceToTarget = distanceToTarget - minRadius

    if (minDistanceToTarget > maxDistanceToTarget) 
    then (Path [])
    else
        let minDistanceToTargetInt = minDistanceToTarget |> int
        let angle = (calculateAngleTo from target) |> round |> int

        match tryFindObstacle heatMap from minDistanceToTargetInt angle with
            | Some obstacle -> obstacle |> Obstacle
            | None ->
            (
                let numberOfTurns = ceil( minDistanceToTarget / (float MAX_SPEED) ) |> int       
                if numberOfTurns = 0 
                then [] |> Path
                else
                    ( 
                        [ for turn in 1 .. numberOfTurns -> 
                            (
                                if (turn = numberOfTurns && (minDistanceToTargetInt % MAX_SPEED) <> 0) 
                                then minDistanceToTargetInt % MAX_SPEED
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

let tryChooseBestPath heatMap (from: Position) (target: Position) (minRadius: float) (maxRadius: float) =
    let rec recursiveTryChooseBestPath heatMap (from: Position) (target: Position) pathSideOption (previousObstacleOption: Entity option) =
        match goForwardOrReturnFirstObstacle heatMap from target minRadius maxRadius with
            | Path path -> Some path       
            | Obstacle obstacle ->
            (
                // find the most useful obstacle (full left & full right)
                let allPossibleObstacles = entitiesCollidingAndSelf heatMap obstacle SHIP_RADIUS
                let orderedPossibleObstaclesByAngle = 
                    allPossibleObstacles 
                    |> List.sortBy (fun o -> cos (calculateRadAngleTo from o.Circle.Position))

                let leftObstacle = orderedPossibleObstaclesByAngle |> List.head
                let rightObstacle = orderedPossibleObstaclesByAngle |> List.last
 
                let useLeftPath = 
                    match (pathSideOption, previousObstacleOption) with
                    | (Some pathSide, Some previousObstacle) 
                        when (pathSide = Right && previousObstacle.Id = leftObstacle.Id) -> false
                    | _ -> true
                let useRightPath = 
                    match (pathSideOption, previousObstacleOption) with
                    | (Some pathSide, Some previousObstacle) 
                        when (pathSide = Left && previousObstacle.Id = rightObstacle.Id) -> false
                    | _ -> true

                // calculate base distance and angle
                let baseDistance = calculateDistanceTo from target
                let baseAngle = calculateAngleTo from target

                // find best path with recursive (tangent of obstacles) 
                let leftPathOption =
                    match useLeftPath with
                    | false -> None
                    | true ->
                    (
                        let intermediateDest = { leftObstacle.Circle with Radius = leftObstacle.Circle.Radius + SHIP_RADIUS }
                        let tangents = circleTangentsFromPoint from intermediateDest
                        let tangent = tangents.[0]

                        let tangentAngle = calculateAngleTo target tangent
                        let leftAngle = ((baseAngle + tangentAngle + 360.0) % 360.0) |> floor

                        // get min length ("from" to "tangent" point length)
                        let minSpeedTangent = ceil(calculateDistanceTo from tangent) |> int

                        // get max length of the tangent (right angle to dest)
                        let maxSpeedTangent = ceil(baseDistance * cos leftAngle) |> int

                        let leftPositionBeforeObstacleOption = tryGetPositionBetween heatMap from minSpeedTangent maxSpeedTangent (int leftAngle)
                        match leftPositionBeforeObstacleOption with
                        | None -> None
                        | Some leftPosition -> recursiveTryChooseBestPath heatMap leftPosition target (Some Left) (Some leftObstacle)
                    )

                let rightPathOption =
                    match useRightPath with
                    | false -> None
                    | true ->
                    (
                        let intermediateDest = { rightObstacle.Circle with Radius = rightObstacle.Circle.Radius + SHIP_RADIUS }
                        let tangents = circleTangentsFromPoint from intermediateDest
                        let tangent = tangents.[1]

                        let tangentAngle = calculateAngleTo target tangent
                        let rightAngle = ((baseAngle + tangentAngle + 360.0) % 360.0) |> ceil

                        // get min length ("from" to "tangent" point length)
                        let minSpeedTangent = ceil(calculateDistanceTo from tangent) |> int

                        // get max length of the tangent (right angle to dest)
                        let maxSpeedTangent = ceil(baseDistance * cos rightAngle) |> int

                        let rightPositionBeforeObstacle = tryGetPositionBetween heatMap from minSpeedTangent maxSpeedTangent (int rightAngle)
                        match rightPositionBeforeObstacle with
                        | None -> None
                        | Some rightPosition -> recursiveTryChooseBestPath heatMap rightPosition target (Some Right) (Some rightObstacle)
                    )

                // find best path from left or right tangent (use the fastest path)
                match (leftPathOption, rightPathOption) with
                    | (None, None) -> None
                    | (Some leftPath, None) -> Some leftPath
                    | (None, Some rightPath) -> Some rightPath
                    | (Some leftPath, Some rightPath) -> 
                        Some (
                            if leftPath.Length <= rightPath.Length 
                            then leftPath 
                            else rightPath
                        )
            )
    recursiveTryChooseBestPath heatMap from target None None

let navigateToPlanet heatMap (ship: Ship) (planet: Planet) =
    // use heat map without own ship
    let heatMapWithoutOwnShip =
        { Entities = heatMap.Entities |> Array.filter (fun e -> e.Id <> ship.Entity.Id) }

    // choose best move orders (path) to go to dest
    let bestPathOption = 
        tryChooseBestPath 
            heatMapWithoutOwnShip 
            ship.Entity.Circle.Position 
            planet.Entity.Circle.Position
            (planet.Entity.Circle.Radius + 1.0)
            (planet.Entity.Circle.Radius + 4.0)

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

let navigateCloseToEnemy heatMap (ship: Ship) (enemyShip: Ship) =
    // use heat map without own ship
    let heatMapWithoutOwnShip =
        { Entities = heatMap.Entities |> Array.filter (fun e -> e.Id <> ship.Entity.Id) }
    
    // choose best move orders (path) to go to dest
    let bestPathOption = 
        tryChooseBestPath 
            heatMapWithoutOwnShip 
            ship.Entity.Circle.Position 
            enemyShip.Entity.Circle.Position
            enemyShip.Entity.Circle.Radius
            WEAPON_RADIUS

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