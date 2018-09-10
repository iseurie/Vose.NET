open System.Diagnostics
open Vose

[<EntryPoint>]
let main _ =
    let skewed = [| 0.2; 0.3; 0.5 |]
    let uniform = [| 0.25; 0.25; 0.25; 0.25 |]
    let testDist dist n =
        let bins = 
            Seq.initInfinite (ignore>>(mkDice dist))
            |> Seq.take n
            |> Seq.groupBy id
            
        let result = 
            [|  let n = Seq.map (snd>>Seq.length) bins |> Seq.sum |> float
                for _, bin in bins do 
                    yield n / (float <| Seq.length bin) |]
        printfn "%A => %A" dist result
    mkDice skewed
    |> (>>) ignore
    |> Seq.initInfinite
    |> Seq.take 1000 |> Seq.averageBy float
    |> printfn "%A"
    0
