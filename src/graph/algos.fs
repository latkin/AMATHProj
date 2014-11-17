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
        let mutable i1 = 0
        while i1 < g.Length do
            Graph.flipEdge i1 g
            let newCliqueCount = Graph.numCliques cliqueSize g
            if newCliqueCount < lowestCliqueCount then
                lowestCliqueCount <- newCliqueCount
                i1 <- Int32.MaxValue
            else
                Graph.flipEdge i1 g
                i1 <- i1 + 1

        lowestCliqueCount


    let makeFirstHelpful2Flip cliqueCount cliqueSize (g:G) =
        let mutable lowestCliqueCount = cliqueCount
        let mutable i1 = 0
        let mutable i2 = 0
        let mutable go = true

        while i1 < g.Length - 1 && go do
            Graph.flipEdge i1 g
            i2 <- i1 + 1
            while i2 < g.Length && go do
                Graph.flipEdge i2 g
                let newCliqueCount = Graph.numCliques cliqueSize g
                if newCliqueCount < lowestCliqueCount then
                    lowestCliqueCount <- newCliqueCount
                    go <- false
                else
                    Graph.flipEdge i2 g
                    i2 <- i2 + 1
            if go then
                Graph.flipEdge i1 g
                i1 <- i1 + 1

        lowestCliqueCount
    
    let rec private continuousImpl level cliqueCount cliqueSize (g:G) =
        let newCliqueCount = 
            match level with
            | 1 -> makeFirstHelpful1Flip cliqueCount cliqueSize g
            | 2 -> makeFirstHelpful2Flip cliqueCount cliqueSize g
            | _ -> cliqueCount

        if newCliqueCount < cliqueCount then
            printfn "New best (lvl %d): %d" level newCliqueCount
            if newCliqueCount = 0 then
                printfn "%s" (Graph.toString Raw g)
                0
            else
                continuousImpl 1 newCliqueCount cliqueSize g
        else
            if level = 3 then cliqueCount else
            printfn "Bumping level"
            continuousImpl (level+1) cliqueCount cliqueSize g

    let continuousScan cliqueCount cliqueSize (g:G) =
        continuousImpl 1 cliqueCount cliqueSize g
