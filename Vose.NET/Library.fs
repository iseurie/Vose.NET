module Vose

open System.Collections.Generic

let mutable rnd = new System.Random ()
        
let private pop (d : Deque<_>) = d.RemoveBack ()

let private drain (d : Deque<_>) =
    seq { while not d.IsEmpty do yield pop d } 
        
let mkDice dist =
    let sides = Array.length dist
    let μ = 1. / float sides
    let n = float sides
    let small, large = new Deque<_>(), new Deque<_>()
    let push (i, p) = (if p < μ then small.Add else large.Add ) i
    do Array.indexed dist |> Array.iter push
    
    let prob = Array.zeroCreate sides
    let alias = Array.zeroCreate sides
     
    for l, g in Seq.zip <| drain small <| drain large do
        prob.[l] <- dist.[l] * n
        alias.[l] <- g
        prob.[g] <- prob.[g] + prob.[l] - μ
        push (g, dist.[l] + dist.[g] - μ)
    
    
    for l in drain large do prob.[l] <- 1.
    for g in drain small do prob.[g] <- 1.
    
    let roll () =
        let i = rnd.Next () % prob.Length
        if rnd.NextDouble () < prob.[i] then i else alias.[i]
    
    roll

