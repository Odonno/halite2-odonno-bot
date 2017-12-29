open System
open Halite
open Logs
open Pathfinding
open Statistics
open Groups

[<EntryPoint>]
let main argv =
    let botName = 
        match argv.Length with 
            | x when x > 0 -> argv.[0] 
            | _ -> "Odonno"

    let conn = newConnection botName

    enableLogs conn.PlayerTag
    flushLogs()

    let mutable groups = [||]
    let mutable gameMap = updateMap conn
    let mutable gameTurn = 1

    while true do
        addLog ("Turn " + (gameTurn |> string) + ":")

        gameMap <- updateMap conn

        // Map data
        let myPlayer = 
            gameMap.Players 
            |> Array.find (fun p -> p.Id = gameMap.MyId)

        let otherPlayers =
            gameMap.Players
            |> Array.filter (fun p -> p.Id <> gameMap.MyId)

        let livingEnemyShips = 
            otherPlayers
            |> Array.map (fun p -> p.Ships)
            |> Array.reduce Array.append
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
            |> Array.map (fun p -> analysePlanet p myPlayer livingEnemyShips biggestPlanet smallestPlanet)

        // Manage groups
        let existingGroups = 
            groups
            |> getLivingGroups gameMap.Planets myPlayer.Ships
            |> getGroupsWithTarget
            |> getGroupsWithUndockedShip

        // order to mine planets (based on search criteria)
        let planetsToConquer = 
            planetStats
            |> Array.filter (fun stat -> stat.CanProduceMore)
            |> Array.sortByDescending (fun stat -> (stat.ProductionInterest + 1.0) * (stat.Risk + 1.0))
            |> Array.map (fun stat -> stat.Planet)

        let newGroups = 
            getUnassignedShips existingGroups myPlayer.Ships
            |> orderNewGroupsFull planetsToConquer

        groups <- Array.append existingGroups newGroups

        // Pathfinding
        let heatMap = createHeatMap gameMap.Planets myPlayer.Ships

        // get commands (move & dock)
        let commandQueue = 
            groups
            |> Array.filter (fun g -> g.Target <> None)
            |> Array.filter (fun g -> g.Ship.DockingStatus = Undocked)
            |> Array.map 
                (fun g -> 
                    let ship = g.Ship

                    match g.Mission with
                    | Some Mining ->
                        match g.Target with
                        | Some planet when canDock ship planet -> dock ship planet
                        | Some planet -> navigateToPlanet heatMap ship planet
                        | None -> ""
                    | _ -> ""                    
                )

        // apply commands & go next turn
        submitCommands commandQueue
        gameTurn <- gameTurn + 1

        addLog ""            

    0 // exit code