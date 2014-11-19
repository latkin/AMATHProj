﻿#load "Control.fs"

open AMATHProj.Lib
#load "Graph.fs"
#load @"C:\Users\latkin\Source\Repos\amathproj\src\tests\TestHelpers.fs"
open AMATHProj.Tests
open AMATHProj.Lib

let g = Graph.random 5 1
let n,cr = g |> Graph.numCliques_Record 3
g |> Graph.flipEdge 0
g|> Graph.cliqueCountForEdge 3 2



g |> Graph.diffCliques 3 0 cr


g |> Graph.numCliques_Record 3

g |> Graph.numCliquesFunctionalFor 5
g |> Graph.numCliques 5

g |> TestHelpers.num5CliquesSimple

for __ = 1 to 10 do
    let g = Graph.random 43 2
    let x1 = g |> TestHelpers.num4CliquesSimple
    let x2 = g |> Graph.numCliques 4
    if x1 <> x2 then failwithf "not equal %d %d" x1 x2
    else printfn "equal"