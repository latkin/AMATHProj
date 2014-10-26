#load "Graph.fs"

open AMATHProj.Lib

let size = 20
let g = Graph.init size 0
g |> Graph.numCliques 5 size