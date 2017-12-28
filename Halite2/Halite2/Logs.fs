module Logs

open System.IO
open System.Runtime.Serialization.Json
open System.Text

let mutable enabled = false
let mutable playerId = -1

let json<'t> (myObj:'t) =   
    use ms = new MemoryStream() 
    (DataContractJsonSerializer(typeof<'t>)).WriteObject(ms, myObj) 
    Encoding.Default.GetString(ms.ToArray()) 

let enableLogs id = 
    enabled <- true
    playerId <- id

let disableLogs () = 
    enabled <- true
    playerId <- -1

let flushLogs () =
    File.WriteAllText("logs/test-" + (playerId |> string) + ".log", "")

let addLog text =
    if enabled
    then File.AppendAllLines("logs/test-" + (playerId |> string) + ".log", [| text |]) 