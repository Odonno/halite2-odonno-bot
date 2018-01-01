open System
open Halite2.Main

[<EntryPoint>]
let main argv =
    let botName = 
        match argv.Length with 
            | x when x > 0 -> argv.[0] 
            | _ -> "Odonno"

    execute botName

    0 // exit code