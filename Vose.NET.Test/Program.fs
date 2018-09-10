open System.Diagnostics

[<EntryPoint>]
let main _ =
    let dist = [| 0.25; 0.25; 0.25; 0.25 |]
    let dice = Vose.mkGen dist
    let timer = Stopwatch.StartNew ()
    let mu = Seq.initInfinite (ignore>>dice) |> Seq.take 1_000_000 |> Seq.averageBy float
    timer.Stop () |> ignore
    printfn "found an average of %f in %fms." mu timer.Elapsed.TotalMilliseconds
    0

