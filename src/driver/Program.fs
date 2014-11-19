open AMATHProj.Lib

let g = Graph.random 5 1
let n,cr = g |> Graph.numCliques_Record 3
g |> Graph.flipEdge 0
g |> Graph.cliqueCountForEdge 3 2