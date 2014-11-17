#load "Graph.fs"
#load @"C:\Users\latkin\Source\Repos\amathproj\src\tests\TestHelpers.fs"
open AMATHProj.Lib
open AMATHProj.Tests
let size = 20

for __ = 1 to 10000 do
    let g = Graph.random size 2
    g |> Graph.numCliques 3

for __ = 1 to 10000 do
    let g = Graph.random size 2
    g |> TestHelpers.num3CliquesSimple

let g = Graph.random 1000 5
g |> Graph.numCliques 3
g |> TestHelpers.num3CliquesSimple