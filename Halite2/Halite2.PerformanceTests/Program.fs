[<EntryPoint>]
let main argv =
    let times = 100_000_000

    CollisionsTests.execute times

    0 // exit code