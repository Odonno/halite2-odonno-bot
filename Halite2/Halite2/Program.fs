open System
open Halite
open Logs
open Pathfinding
open Statistics
open Groups

// TODO : Pathfinding - use second/third... obstacle path even if there is an obstacle between ship & the tangent
// TODO : Pathfinding - create an Obstacle Id instead of using Entity Id (differentiating obstacles from moves)
// TODO : Stats - player stats (to get realtime ranking)
// TODO : Groups - improve mining (better matching of closest group to planets)
// TODO : Groups - improve attack (better matching of closest group to enemy ships)
// TODO : Groups - defensive strategy (protect mining ships) when danger
// TODO : Groups - unassign mining group if planet is already full mined
// TODO : Chore - Redux pattern
// TODO : Logs - disable logs on Release mode

[<EntryPoint>]
let main argv =
    let botName = 
        match argv.Length with 
            | x when x > 0 -> argv.[0] 
            | _ -> "Odonno"

    // initialize game state
    let conn = (newConnection botName).Value
    updateMap conn |> ignore

    enableLogs conn.PlayerTag
    flushLogs()

    let mutable groups = [||]
    let mutable gameTurn = 1

    try
        while true do
            addLog ("Turn " + (gameTurn |> string) + ":")

            match updateMap conn with
                | None -> 
                    (
                        addLog ("Terminated on turn " + (gameTurn |> string))
                        Environment.Exit 1
                    )
                | Some gameMap ->
                (
                    // Map data
                    let myPlayer = 
                        gameMap.Players 
                        |> Array.find (fun p -> p.Id = gameMap.MyId)

                    let otherPlayers =
                        gameMap.Players
                        |> Array.filter (fun p -> p.Id <> gameMap.MyId)

                    let enemyShips =
                        otherPlayers
                        |> Array.map (fun p -> p.Ships)
                        |> Array.reduce Array.append

                    let undockedEnemyShips = 
                        enemyShips
                        |> Array.filter (fun s -> s.DockingStatus = DockingStatus.Undocked)

                    let biggestPlanet = 
                        gameMap.Planets 
                        |> Array.sortByDescending (fun p -> p.Entity.Circle.Radius) 
                        |> Array.head

                    let smallestPlanet = 
                        gameMap.Planets 
                        |> Array.sortBy (fun p -> p.Entity.Circle.Radius)
                        |> Array.head

                    // Analyse / Stats
                    
                    let planetStats = // Planet stats
                        gameMap.Planets 
                        |> Array.map (fun p -> analysePlanet p myPlayer undockedEnemyShips biggestPlanet smallestPlanet)

                    // Manage groups
                    let existingGroups = 
                        groups
                        |> getLivingGroups gameMap.Planets enemyShips myPlayer.Ships
                        |> getGroupsWithTarget

                    // order to mine planets (based on search criteria)
                    let planetsStatsToMine = 
                        planetStats
                        |> Array.filter (fun stat -> stat.CanProduceMore)
                        |> Array.filter (fun stat -> not stat.IsEnemy)
                        |> Array.sortByDescending (fun stat -> (stat.ProductionInterest + 1.0) * (stat.Risk + 1.0))

                    let newMiningGroups = 
                        getUnassignedShips existingGroups myPlayer.Ships
                        |> orderNewGroupsSmartMining existingGroups planetsStatsToMine

                    let groups2 = Array.append existingGroups newMiningGroups

                    // order to attack enemy ships
                    let newAttackGroups = 
                        getUnassignedShips groups2 myPlayer.Ships
                        |> orderNewGroupsDumbAttack enemyShips

                    groups <- Array.append groups2 newAttackGroups

                    // Pathfinding
                    let heatMapTurns = 50
                    let mutable heatMap = createHeatMap heatMapTurns gameMap.Planets myPlayer.Ships

                    // get commands (move & dock)
                    let commandQueue = 
                        groups
                        |> getGroupsWithUndockedShip
                        |> getGroupsWithTarget
                        |> Array.map 
                            (fun g -> 
                                let ship = g.Ship

                                match g.Mission with
                                | Some Mining ->
                                    match g.Target with
                                    | Some (Planet planet) when canDock ship planet -> dock ship planet
                                    | Some (Planet planet) -> 
                                        (
                                            let updatedheatMap, command = navigateToPlanet heatMap ship planet
                                            heatMap <- updatedheatMap

                                            command
                                        )                                 
                                    | _ -> ""
                                | Some Attack ->
                                    match g.Target with
                                    | Some (Ship enemyShip) when not (canFight ship enemyShip) -> 
                                        (
                                            let updatedheatMap, command = navigateCloseToEnemy heatMap ship enemyShip
                                            heatMap <- updatedheatMap

                                            command
                                        )                                    
                                    | _ -> ""
                                | _ -> ""                    
                            )

                    // apply commands & go next turn
                    submitCommands commandQueue
                )

            gameTurn <- gameTurn + 1

            addLog ""            
    with
        | ex -> 
        (
            addLog ex.Message
            addLog ""
            addLog ex.StackTrace
        )

    0 // exit code