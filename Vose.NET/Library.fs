module Vose
open FSharpPlus

let mutable rnd = new System.Random ()

let mkGen sides dist =
    // scale our probabilities
    let scaled = Array.map ((*) (float sides) >> int) dist
    let p scaled = float scaled / float sides
    let grp s =
        let strata = Seq.groupBy (p >> (<) 1.) s
        let small = strata |> Seq.find fst |> snd
        let large = strata |> Seq.find (fst>>not) |> snd
        small, large
    let prob = Array.zeroCreate sides
    let alias = Array.zeroCreate sides
    let small, large = grp scaled
    let ps = 
        seq { for l, g in Seq.zip small large do
                prob.[l] <- scaled.[l]
                alias.[l] <- g
                yield (scaled.[g] + scaled.[l] - 1) }
    let small, large =
        let ls, gs = grp ps
        Seq.append small ls,
        Seq.append large gs
    for g in large do prob.[g] <- 1
    for l in small do prob.[l] <- 1
    
    let gen () =
        let i = rnd.Next () % sides
        if p prob.[i] * rnd.NextDouble () > 0.5 then i else alias.[i]
  
    gen
