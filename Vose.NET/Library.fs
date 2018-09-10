module Vose

open System.Collections.Generic

let mutable rnd = new System.Random ()
        
let private drain (d : Deque<_>) = seq {
        while d.IsFull do yield d.RemoveFront () }
        
let mkDice dist =
    let sides = Array.length dist
    let mu = 1. / float sides
    let small, large = new Deque<_>(), new Deque<_>()
    let smallp p = p < mu
    let push (i, p) = (if smallp p then small.AddBack else large.AddBack) i
    for p in Array.indexed dist do push p
    let prob = Array.zeroCreate sides
    let alias = Array.zeroCreate sides
    
    for l, g in Seq.zip <| drain small <| drain large do
        prob.[l] <- dist.[l] * float sides 
        alias.[l] <- g
        push (g, dist.[l] + dist.[g] - mu)
    
    for l in drain large do prob.[l] <- 1.
    for g in drain small do prob.[g] <- 1.
    
    let roll () =
        let i = rnd.Next () % prob.Length
        if rnd.NextDouble () < prob.[i] then i else alias.[i]
    
    roll

