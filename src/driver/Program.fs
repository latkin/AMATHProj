open AMATHProj.Lib

let numVtx = 43
let numColors = 2
let cliqueSize = 5

let g = Graph.random numVtx numColors

let mutable numCliques = g |> Graph.numCliques cliqueSize
printfn "Starting: %d" numCliques

Algos.continuousScan numCliques cliqueSize g