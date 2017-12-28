module ExecutePerformanceTests

open System
open System.Diagnostics

let executeFunction f times =
    {1..times}
    |> Seq.iter (fun _ -> f())

let logPerfsStart functionName =
    Console.WriteLine("Start execution of " + functionName)

let logPerfsEnd functionName executionTime =
    Console.WriteLine(functionName + " executed in " + (executionTime |> string) + " ms...")

let logPerfs functionName f times =
    logPerfsStart functionName

    let watch = Stopwatch()
    watch.Start()

    executeFunction f times

    watch.Stop()

    logPerfsEnd functionName watch.ElapsedMilliseconds