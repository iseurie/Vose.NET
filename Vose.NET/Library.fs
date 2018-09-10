module Vose
open FSharpPlus

let mutable rnd = new System.Random ()

let mkGen dist =
    let sides = Array.length dist
    let mu = 1. / float sides
    let grp s =
        let strata = Seq.groupBy (snd >> (<) mu) s
        let unwrap = Option.map (snd>>Seq.map fst) >> Option.defaultValue Seq.empty
        let small = strata |> Seq.tryFind fst |> unwrap
        let large = strata |> Seq.tryFind (fst>>not) |> unwrap
        small, large
    let prob = Array.zeroCreate sides
    let alias = Array.zeroCreate sides
    let small, large = grp <| Seq.indexed dist
    let ps = seq {
        for l, g in Seq.zip small large do
            prob.[l] <- dist.[l] * float sides 
            alias.[l] <- g
            yield g, dist.[l] + dist.[g] - mu }
    let small, large =
        let ls, gs = grp ps
        Seq.append small ls,
        Seq.append large gs
    for g in large do prob.[g] <- 1.
    for l in small do prob.[l] <- 1.
    
    let dice () =
        let i = rnd.Next () % sides
        if prob.[i] * rnd.NextDouble () > 0.5 then i else alias.[i]
  
    dice


