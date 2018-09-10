[<EntryPoint>]
let main _ =
    let gen = Vose.mkGen [| 0.2; 0.3; 0.5 |]
    Seq.initInfinite (ignore>>gen) |> Seq.take 30000 |> Seq.averageBy float
    |> printfn "%A"
    0

