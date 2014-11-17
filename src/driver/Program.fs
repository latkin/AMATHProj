open AMATHProj.Lib


let numVtx = 25
let numColors = 2
let cliqueSize = 5

let g = Graph.random numVtx numColors

let mutable numCliques = g |> Graph.numCliques cliqueSize
printfn "Starting: %d" numCliques

let mutable newNum = Algos.makeFirstHelpful1Flip numCliques cliqueSize g
while newNum < numCliques do
    printfn "New lowest: %d" newNum
    numCliques <- newNum
    newNum <- Algos.makeFirstHelpful1Flip numCliques cliqueSize g
