open AMATHProj.Lib

let g = Graph.random 35 2

let reducedCount = Algos.continuousScan 5 g
printfn "Calculated: %d" reducedCount
g |> Graph.numCliques 5 |> (printfn "Check: %d")
