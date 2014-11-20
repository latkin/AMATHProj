open AMATHProj.Lib
open System.Threading.Tasks
open System
DateTime.Now |> printfn "%O"
let mutable go = true
Graph.populateVertInfo 43 |> ignore
let rnd = Random()
[|1|]
|> Array.map (fun i -> Task.Run(fun _ ->
    let g = Graph.random 34 2
    let mutable min = 10000
    let mutable maxScan = 3
    while go do
        let reducedCount = Algos.continuousScan maxScan 5 g

   //     let x = rnd.NextDouble()
   //     if x > 0.1 then maxScan <- 2
   //     elif x > 0.001 then maxScan <- 1
   //     else maxScan <- 3
      //  let check = g |> Graph.numCliques 5
     //   if reducedCount <> check then failwith "Miss"

   //     if reducedCount < min then
    //        min <- reducedCount
    //        printfn "Calculated (%d): %d" i reducedCount
   //         if min < 10 then maxScan <- 3
     //   g |> Graph.numCliques 4 |> (printfn "Check: %d")
        if reducedCount = 0 then
            g |> Graph.toString Raw |> printfn "%s"
            go <- false
        else
            for j = 1 to 3 do
                let i = rnd.Next(g.Length)
                Graph.flipEdge i g
            ))
|> Task.WaitAll
DateTime.Now |> printfn "%O"
