#load "Graph.fs"
#load @"C:\Users\latkin\Source\Repos\amathproj\src\tests\TestHelpers.fs"
open AMATHProj.Lib
open AMATHProj.Tests
let size = 43

for __ = 1 to 10000 do
    let g = Graph.random size 2
    g |> Graph.numCliques 5 size

for __ = 1 to 10000 do
    let g = Graph.random size 2
    g |> TestHelpers.num5CliquesSimple size