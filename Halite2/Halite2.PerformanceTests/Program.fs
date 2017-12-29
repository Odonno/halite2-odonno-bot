[<EntryPoint>]
let main argv =
    let times = 100_000_000

    //CollisionsTests.executeAll times
    PathfindingTests.executeAll times

    0 // exit code