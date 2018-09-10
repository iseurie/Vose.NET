open System.Diagnostics
open Vose

[<EntryPoint>]
let main _ =
    let skewed = [| 0.2; 0.3; 0.5 |]
    let uniform = [| 0.25; 0.25; 0.25; 0.25 |]
    let timer = Stopwatch.StartNew ()
    let avg dist n =
        Seq.initInfinite (ignore>>(mkGen dist))
        |> Seq.take n
        |> Seq.averageBy (Array.get dist)
    let mu = avg skewed 10_000_000
    timer.Stop () |> ignore
    printfn "found an average of %f in %fms." mu timer.Elapsed.TotalMilliseconds
    0
