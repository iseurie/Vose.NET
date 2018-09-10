open System.Diagnostics
open Vose

[<EntryPoint>]
let main _ =
    let skewed = [| 0.2; 0.3; 0.5 |]
    let uniform = [| 0.25; 0.25; 0.25; 0.25 |]
    let timer = Stopwatch.StartNew ()
    let testDist dist n =
        let bins = 
            Seq.initInfinite (ignore>>(mkGen dist))
            |> Seq.groupBy id
            
        let result = 
            [|  let n = Seq.map (snd>>Seq.length) bins |> Seq.sum |> float
                for _, bin in bins do 
                    yield n / (float <| Seq.length bin) |]
        printfn "%A => %A" dist result
    testDist skewed 100_000
    testDist uniform 100_000
    0
