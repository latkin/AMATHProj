namespace AMATHProj.Lib
open System

module Algos =
    let makeBest1Flip cliqueCount cliqueSize (g:G) =
        let mutable lowestCliqueCount = cliqueCount
        let mutable bestEdge = -1

        for i = 0 to (g.Length - 1) do
            Graph.flipEdge i g
            let newCliqueCount = Graph.numCliques cliqueSize g
            if newCliqueCount < lowestCliqueCount then
                lowestCliqueCount <- newCliqueCount
                bestEdge <- i
            Graph.flipEdge i g

        if bestEdge <> -1 then
            Graph.flipEdge bestEdge g
        lowestCliqueCount

    let makeFirstHelpful1Flip cliqueCount cliqueSize (g:G) =
        let mutable lowestCliqueCount = cliqueCount
        let mutable i = 0
        while i < g.Length do
            Graph.flipEdge i g
            let newCliqueCount = Graph.Parallel.numCliques cliqueSize g
            if newCliqueCount < lowestCliqueCount then
                lowestCliqueCount <- newCliqueCount
                i <- Int32.MaxValue
            else
                Graph.flipEdge i g
                i <- i + 1

        lowestCliqueCount
