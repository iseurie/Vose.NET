open System.Diagnostics
open Vose

[<EntryPoint>]
let main _ =
    let skewed = [| 0.2; 0.3; 0.5 |]
    let uniform = [| 0.25; 0.25; 0.25; 0.25 |]
    let testDist n dist =
        let bins = 
            Seq.initInfinite (ignore>>(mkDice dist))
            |> Seq.take n
            |> Seq.groupBy id
            
        let result = 
            [|  let n = Seq.map (snd>>Seq.length) bins |> Seq.sum |> float
                for _, bin in Seq.sortBy fst bins do 
                    yield (float <| Seq.length bin) / n |]
        printfn "%A => %A" dist result
    [ skewed; uniform ] |> Seq.iter (testDist 10_000_000) |> ignore
    0
