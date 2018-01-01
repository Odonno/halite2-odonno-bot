module Pathfinding

open Constants
open Collisions
open Halite
open Logs
open System

type HeatMapTurn = {
    Turn: int;
    Entities: Entity[];
}

type HeatMap = HeatMapTurn list

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

type PositionOrObstacle =
    | Position of Position
    | Obstacle of Entity

let calculateNewPosition (position: Position) distance angle =
    let radAngle = degreesToRadians (float angle)
    let x = position.X + distance * cos radAngle
    let y = position.Y + distance * sin radAngle
    { X = x; Y = y }

let entitiesCollidingAndSelf (heatMapTurn: HeatMapTurn) entity expandedRadius =
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

    recursiveEntitiesCollidingAndSelf entity (heatMapTurn.Entities |> Array.toList) []

let createHeatMap turns (planets: Planet[]) (myShips: Ship[]) = 
    let planetEntities = 
        planets
        |> Array.map (fun p -> p.Entity)

    let myShipEntities = 
        myShips
        |> Array.map (fun s -> s.Entity)

    let myDockedShipEntities = 
        myShips
        |> Array.filter (fun s -> s.DockingStatus <> Undocked)
        |> Array.map (fun s -> s.Entity)

    [1..turns]
    |> List.map 
        (fun turn ->
            if (turn = 1)
            then
                {
                    Turn = turn;
                    Entities = Array.append planetEntities myShipEntities
                }
            else
                {
                    Turn = turn;
                    Entities = Array.append planetEntities myDockedShipEntities
                }
        )

let getHeatMapTurn turn heatMap =
    heatMap |> List.filter (fun heatMapTurn -> heatMapTurn.Turn = turn) |> List.tryHead

let updateHeatMapWithNewPath heatMap (ship: Ship) (path: Path) =
    let mutable futurePosition = ship.Entity.Circle.Position

    heatMap
    |> List.map 
        (fun turnHeatMap ->
            if turnHeatMap.Turn > path.Length
            then turnHeatMap
            else
                let moveOrder = path.[turnHeatMap.Turn - 1]
                let moveEntities =
                    [| 1..(moveOrder.Speed) |]
                    |> Array.map 
                        (fun _ ->
                            futurePosition <- calculateNewPosition futurePosition 1.0 moveOrder.Angle
                            let circle = { Position = futurePosition; Radius = SHIP_RADIUS; }
                            { ship.Entity with Circle = circle; }
                        )

                { turnHeatMap with Entities = Array.append turnHeatMap.Entities moveEntities }
        )

let updateHeatMapWithStationaryShip heatMap (ship: Ship) =
    heatMap
    |> List.map 
        (fun heatMapTurn ->
            if heatMapTurn.Turn = 1
            then heatMapTurn
            else 
                { heatMapTurn with Entities = Array.append heatMapTurn.Entities [| ship.Entity |] }
        )

let calculateNumberOfTurns speed =
    (float speed / 7.0) |> ceil |> int

let canFight (ship: Ship) (enemyShip: Ship) =
    let distance = calculateDistanceTo ship.Entity.Circle.Position enemyShip.Entity.Circle.Position
    distance <= WEAPON_RADIUS

let tryFindObstacle (startTurn: int) (exceptEntity: Entity) (heatMap: HeatMap) (from: Position) speed angle = 
    [1.0..(float speed)]
    |> Seq.tryPick 
        (fun s ->
            let futurePosition = calculateNewPosition from s angle
            let turn = startTurn - 1 + (calculateNumberOfTurns (int s))

            let heatMapTurnOption = (heatMap |> getHeatMapTurn turn)
            match heatMapTurnOption with
            | None -> None // TODO : to fix
            | Some heatMapTurn ->
            (
                heatMapTurn.Entities
                    |> Array.filter (fun e -> e.Id <> exceptEntity.Id)
                    |> Array.tryFind 
                        (fun e -> 
                            circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                        ) 
            )          
        )

let goToOrReturnFirstObstacle (startTurn: int) (exceptEntity: Entity) (heatMap: HeatMap) (from: Position) (target: Position) (angle: int) (minRadius: float) (maxRadius: float) =
    let distanceToTarget = calculateDistanceTo from target

    let minDistanceToTarget = ceil(distanceToTarget - maxRadius)
    let maxDistanceToTarget = distanceToTarget - minRadius

    if (minDistanceToTarget > maxDistanceToTarget) 
    then (Path [])
    else
        let minDistanceToTargetInt = minDistanceToTarget |> int

        match tryFindObstacle startTurn exceptEntity heatMap from minDistanceToTargetInt angle with
            | Some obstacle -> obstacle |> PathOrObstacle.Obstacle
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
                    ) |> PathOrObstacle.Path
            )  

let getPositionBetweenOrObstacle (startTurn: int) (exceptEntity: Entity) (heatMap: HeatMap) from minSpeed maxSpeed angle =
    [1..maxSpeed]
    |> Seq.tryPick 
        (fun s ->
            let futurePosition = calculateNewPosition from (float s) angle
            let turn = startTurn - 1 + (calculateNumberOfTurns s)
            
            let heatMapTurnOption = (heatMap |> getHeatMapTurn turn)
            match heatMapTurnOption with
            | None -> None
            | Some heatMapTurn ->
            (
                let obstacleOption = 
                    heatMapTurn.Entities
                        |> Array.filter (fun e -> e.Id <> exceptEntity.Id)
                        |> Array.tryFind 
                            (fun e -> 
                                circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                            )
                
                match obstacleOption with
                    | None -> 
                    (
                        if s < minSpeed
                        then None
                        else Some (s, futurePosition |> PositionOrObstacle.Position)
                    )              
                    | Some obstacle -> Some (s, obstacle |> PositionOrObstacle.Obstacle) 
            )                
        ) 

let createMoveOrders speed angle =
    let numberOfTurns = calculateNumberOfTurns speed
    [1..numberOfTurns]
    |> List.map 
        (fun turn ->
            if (turn = numberOfTurns)
            then { Speed = speed % 7; Angle = angle }
            else { Speed = 7; Angle = angle }
        )

let getTangentsFromObstacle currentTurn heatMap obstacle (ship: Ship) from target baseDistance =
    let intermediateDest = { obstacle.Circle with Radius = obstacle.Circle.Radius + SHIP_RADIUS }
    let angleToObstacle = calculateAngleTo from obstacle.Circle.Position
    let tangents = circleTangentsFromPoint from intermediateDest

    // left tangent
    let leftTangent = tangents.[0]
    let leftTangentAngle = (calculateAngleTo target leftTangent)
    let leftAngle = ((angleToObstacle + leftTangentAngle + 360.0) % 360.0) |> floor |> int

    // right tangent
    let rightTangent = tangents.[1]
    let rightTangentAngle = (calculateAngleTo target rightTangent)
    let rightAngle = ((angleToObstacle + rightTangentAngle + 360.0) % 360.0) |> ceil |> int

    // get min length ("from" to "tangent" point length)
    let minSpeedTangent = ceil(calculateDistanceTo from leftTangent) |> int

    // get max length of the tangent (right angle to dest)
    let maxSpeedTangent = ceil(baseDistance * cos (float leftAngle)) |> int

    // get speed and position for each possible tangent
    let leftTangentOption =
        match getPositionBetweenOrObstacle currentTurn ship.Entity heatMap from minSpeedTangent maxSpeedTangent (int leftAngle) with
        | Some (speed, PositionOrObstacle.Position position) -> Some (speed, position)
        | _ -> None

    let rightTangentOption =
        match getPositionBetweenOrObstacle currentTurn ship.Entity heatMap from minSpeedTangent maxSpeedTangent (int rightAngle) with
        | Some (speed, PositionOrObstacle.Position position) -> Some (speed, position)
        | _ -> None

    match (leftTangentOption, rightTangentOption) with
        | (Some leftTangent, Some rightTangent) -> [leftTangent; rightTangent]
        | (Some leftTangent, None) -> [leftTangent]
        | (None, Some rightTangent) -> [rightTangent]
        | (None, None) -> []

let tryFindBestPath (paths: Path list) =
    paths
    |> List.sortBy (fun orders -> orders.Length, orders |> List.sumBy (fun o -> o.Speed))
    |> List.tryHead

let tryToGoForwardSimple (startTurn: int) heatMap (ship: Ship) (angle: int) =
    [1..7]
    |> Seq.takeWhile 
        (fun s ->
            let futurePosition = calculateNewPosition ship.Entity.Circle.Position (float s) angle

            let heatMapTurnOption = (heatMap |> getHeatMapTurn startTurn)
            match heatMapTurnOption with
            | None -> false // TODO : to fix
            | Some heatMapTurn ->
            (
                heatMapTurn.Entities
                |> Array.filter (fun e -> e.Id <> ship.Entity.Id)
                |> Array.exists 
                    (fun e -> 
                        circlesCollide ({ Position = futurePosition; Radius = SHIP_RADIUS }) e.Circle
                    )
                |> not                    
            )          
        )
    |> Seq.tryLast
    |>  (fun speedOption -> 
            match speedOption with
            | Some speed -> Some [{ Speed = speed; Angle = angle; }]
            | None -> None
        )    

let tryChooseBestPathAlt (heatMap: HeatMap) (ship: Ship) (target: Position) (minRadius: float) (maxRadius: float) =
    let rec recursiveTryChooseBestPath (currentTurn: int) heatMap (from: Position) (target: Position) (parentPath: Path) =
        if (parentPath.Length > 4)
        then None
        else
            // calculate base angle
            let baseAngle = calculateAngleTo from target |> round |> int

            // try to forward to target
            match goToOrReturnFirstObstacle currentTurn ship.Entity heatMap from target baseAngle minRadius maxRadius with
                | Path path -> Some (parentPath @ path)       
                | _ ->
                    (
                        // calculate base distance
                        let baseDistance = calculateDistanceTo from target

                        // get perimeter (circle between ship and target)
                        let circlePerimeter = createCircleFromPoints from target

                        // TODO : fix by filtering 1 move order for closest obstacles 1 and 2 orders for far of 2

                        // find closest obstacles (in the perimeter)
                        let closestObstaclesOneTurnFar =
                            match (heatMap |> getHeatMapTurn currentTurn) with
                                | None -> [||]
                                | Some heatMapTurn -> 
                                (
                                    heatMapTurn.Entities
                                    |> Array.filter (fun e -> e.Id <> ship.Entity.Id)
                                    |> Array.filter (fun e -> circlesCollide circlePerimeter e.Circle)
                                )

                        let closestObstaclesTwoTurnsFar =
                            match (heatMap |> getHeatMapTurn (currentTurn + 1)) with
                                | None -> [||]
                                | Some heatMapTurn -> 
                                (
                                    heatMapTurn.Entities
                                    |> Array.filter (fun e -> e.Id <> ship.Entity.Id)
                                    |> Array.filter (fun e -> circlesCollide circlePerimeter e.Circle)
                                )

                        let closestObstacles = Array.append closestObstaclesOneTurnFar closestObstaclesTwoTurnsFar

                        // find left and right path for each obstacle
                        let tangents =
                            closestObstacles
                            |> Array.map 
                                (fun o ->
                                    getTangentsFromObstacle 
                                        currentTurn
                                        heatMap
                                        o
                                        ship
                                        from
                                        target
                                        baseDistance
                                )
                            |> Array.reduce List.append

                        // continue recursion by using intermediate destination (only for points in perimeter)
                        let tangentsInPerimeter =
                            tangents
                            |> List.filter (fun (_, intermediatePosition) -> isInCircle intermediatePosition circlePerimeter)

                        let bestPathFoundOption =
                            tangentsInPerimeter
                            |> List.map 
                                (fun (speed, intermediatePosition) ->
                                    let moveOrders = createMoveOrders speed baseAngle

                                    recursiveTryChooseBestPath
                                        (currentTurn + moveOrders.Length)
                                        heatMap 
                                        intermediatePosition 
                                        target
                                        (parentPath @ moveOrders)
                                )
                            |> List.filter (fun x -> x.IsSome)
                            |> List.map (fun x -> x.Value)  
                            |> tryFindBestPath // choose fastest path from this list of path

                        if (parentPath.Length <= 0 && bestPathFoundOption.IsNone)
                        then 
                            match tryToGoForwardSimple currentTurn heatMap ship baseAngle with
                            | Some path -> Some path
                            | None ->
                            (
                                match tryToGoForwardSimple currentTurn heatMap ship (baseAngle + 45) with
                                | Some path -> Some path
                                | None ->
                                (
                                    match tryToGoForwardSimple currentTurn heatMap ship (baseAngle - 45) with
                                    | Some path -> Some path
                                    | None -> None
                                )
                            )
                        else bestPathFoundOption
                    )

    recursiveTryChooseBestPath 1 heatMap ship.Entity.Circle.Position target []

let tryChooseBestPath (heatMap: HeatMap) (ship: Ship) (target: Position) (minRadius: float) (maxRadius: float) =
    let rec recursiveTryChooseBestPath (currentTurn: int) heatMap (from: Position) (target: Position) (angleOption: float option) pathSideOption (previousObstacleOption: Entity option) (parentPath: Path) =
        if (parentPath.Length > 20) 
        then None
        else
            let baseAngle = 
                match angleOption with
                    | None -> calculateAngleTo from target
                    | Some angle -> angle
                |> round |> int

            match goToOrReturnFirstObstacle currentTurn ship.Entity heatMap from target baseAngle minRadius maxRadius with
                | Path path -> Some (parentPath @ path)       
                | PathOrObstacle.Obstacle obstacle ->
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

                    // calculate base distance and angle
                    let baseDistance = calculateDistanceTo from target
                    let angleToObstacle = calculateAngleTo from obstacle.Circle.Position

                    // find best path with recursive (tangent of obstacles) 
                    let leftPathOption =
                        match useLeftPath with
                        | false -> None
                        | true ->
                        (
                            let intermediateDest = { obstacle.Circle with Radius = obstacle.Circle.Radius + SHIP_RADIUS }
                            let tangents = circleTangentsFromPoint from intermediateDest
                            let tangent = tangents.[0]

                            let tangentAngle = (calculateAngleTo target tangent)
                            let leftAngle = ((angleToObstacle + tangentAngle + 360.0) % 360.0)  |> floor |> int

                            // get min length ("from" to "tangent" point length)
                            let minSpeedTangent = ceil(calculateDistanceTo from tangent) |> int

                            // get max length of the tangent (right angle to dest)
                            let maxSpeedTangent = ceil(baseDistance * cos (float leftAngle)) |> int

                            match getPositionBetweenOrObstacle currentTurn ship.Entity heatMap from minSpeedTangent maxSpeedTangent (int leftAngle) with
                            | None -> None
                            | Some (_, PositionOrObstacle.Obstacle leftObstacle) -> 
                                (
                                    None
                                    // let newAngle = calculateAngleTo from leftObstacle.Circle.Position

                                    // if (sin (degreesToRadians newAngle)) < 0.0
                                    // then None
                                    // else
                                    //     recursiveTryChooseBestPath 
                                    //         currentTurn 
                                    //         heatMap 
                                    //         from 
                                    //         target 
                                    //         (Some newAngle)
                                    //         (Some Left) 
                                    //         (Some leftObstacle)
                                    //         parentPath
                                )
                            | Some (speed, PositionOrObstacle.Position leftPosition) -> 
                                (
                                    let moveOrders = createMoveOrders speed baseAngle

                                    recursiveTryChooseBestPath 
                                        (currentTurn + moveOrders.Length)
                                        heatMap 
                                        leftPosition 
                                        target 
                                        None
                                        (Some Left) 
                                        (Some obstacle)
                                        (parentPath @ moveOrders)
                                )                        
                        )

                    let rightPathOption =
                        match useRightPath with
                        | false -> None
                        | true ->
                        (
                            let intermediateDest = { obstacle.Circle with Radius = obstacle.Circle.Radius + SHIP_RADIUS }
                            let tangents = circleTangentsFromPoint from intermediateDest
                            let tangent = tangents.[1]

                            let tangentAngle = (calculateAngleTo target tangent)
                            let rightAngle = ((angleToObstacle + tangentAngle + 360.0) % 360.0) |> ceil |> int

                            // get min length ("from" to "tangent" point length)
                            let minSpeedTangent = ceil(calculateDistanceTo from tangent) |> int

                            // get max length of the tangent (right angle to dest)
                            let maxSpeedTangent = ceil(baseDistance * cos (float rightAngle)) |> int

                            match getPositionBetweenOrObstacle currentTurn ship.Entity heatMap from minSpeedTangent maxSpeedTangent (int rightAngle) with
                            | None -> None
                            | Some (_, PositionOrObstacle.Obstacle rightObstacle) -> 
                                (
                                    None
                                    // let newAngle = calculateAngleTo from rightObstacle.Circle.Position
                                    
                                    // if (sin (degreesToRadians newAngle)) < 0.0
                                    // then None
                                    // else
                                    //     recursiveTryChooseBestPath 
                                    //         currentTurn 
                                    //         heatMap 
                                    //         from 
                                    //         target 
                                    //         (Some newAngle)
                                    //         (Some Right) 
                                    //         (Some rightObstacle)
                                    //         parentPath
                                )
                            | Some (speed, PositionOrObstacle.Position rightPosition) -> 
                                (
                                    let moveOrders = createMoveOrders speed baseAngle

                                    recursiveTryChooseBestPath 
                                        (currentTurn + moveOrders.Length)
                                        heatMap 
                                        rightPosition 
                                        target 
                                        None
                                        (Some Right) 
                                        (Some obstacle)
                                        (parentPath @ moveOrders)
                                ) 
                        )

                    // find best path from left or right tangent (use the fastest path)
                    let bestPathFoundOption =
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

                    // try basic move if no better path
                    if (parentPath.Length <= 0 && bestPathFoundOption.IsNone)
                    then 
                        match tryToGoForwardSimple currentTurn heatMap ship baseAngle with
                        | Some path -> Some path
                        | None ->
                        (
                            match tryToGoForwardSimple currentTurn heatMap ship (baseAngle + 45) with
                            | Some path -> Some path
                            | None ->
                            (
                                match tryToGoForwardSimple currentTurn heatMap ship (baseAngle - 45) with
                                | Some path -> Some path
                                | None -> 
                                (
                                    match tryToGoForwardSimple currentTurn heatMap ship (baseAngle + 90) with
                                    | Some path -> Some path
                                    | None -> 
                                    (
                                        match tryToGoForwardSimple currentTurn heatMap ship (baseAngle - 90) with
                                        | Some path -> Some path
                                        | None -> None
                                    )
                                )
                            )
                        )
                    else bestPathFoundOption
                )

    recursiveTryChooseBestPath 1 heatMap ship.Entity.Circle.Position target None None None []

let navigateToPlanet (heatMap: HeatMap) (ship: Ship) (planet: Planet) =
    // choose best move orders (path) to go to dest
    let bestPathOption = 
        tryChooseBestPath 
            heatMap 
            ship 
            planet.Entity.Circle.Position
            (planet.Entity.Circle.Radius + 1.0)
            (planet.Entity.Circle.Radius + 4.0)

    match bestPathOption with
        | None -> (updateHeatMapWithStationaryShip heatMap ship, "")
        | Some [] -> (updateHeatMapWithStationaryShip heatMap ship, "")
        | Some bestPath ->
        (
            // update heatmap based on orders
            let newHeatMap = updateHeatMapWithNewPath heatMap ship bestPath

            // move using the first order
            let firstOrder = bestPath.[0]
            let command = thrust ship firstOrder.Speed firstOrder.Angle

            (newHeatMap, command)
        )

let navigateCloseToEnemy (heatMap: HeatMap) (ship: Ship) (enemyShip: Ship) =
    // choose best move orders (path) to go to dest
    let bestPathOption = 
        tryChooseBestPath 
            heatMap 
            ship 
            enemyShip.Entity.Circle.Position
            enemyShip.Entity.Circle.Radius
            WEAPON_RADIUS

    match bestPathOption with
        | None -> (updateHeatMapWithStationaryShip heatMap ship, "")
        | Some [] -> (updateHeatMapWithStationaryShip heatMap ship, "")
        | Some bestPath ->
        (
            // update heatmap based on orders
            let newHeatMap = updateHeatMapWithNewPath heatMap ship bestPath

            // move using the first order
            let firstOrder = bestPath.[0]
            let command = thrust ship firstOrder.Speed firstOrder.Angle

            (newHeatMap, command)
        )