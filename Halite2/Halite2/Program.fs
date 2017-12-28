open System
open Halite
open Logs
open Pathfinding

[<EntryPoint>]
let main argv =
    let botName = 
        match argv.Length with 
            | x when x > 0 -> argv.[0] 
            | _ -> "Odonno"

    let conn = newConnection botName

    enableLogs conn.PlayerTag
    flushLogs()

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

        // Pathfinding
        let heatmap = createHeatMap gameMap myPlayer

        // get commands (move & dock)
        let commandQueue = [| "" |]

        // apply commands & go next turn
        submitCommands commandQueue
        gameTurn <- gameTurn + 1

        addLog ""            

    0 // exit code