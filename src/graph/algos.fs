namespace AMATHProj.Lib
open System

module Algos =
    let makeBest1Flip cliqueSize cliqueCount (cliqueRecord:CR) (g:G) =
        let mutable lowestCliqueCount = cliqueCount
        let mutable bestEdge = -1

        for i = 0 to (g.Length - 1) do
            if cliqueCount - cliqueRecord.[i] >= lowestCliqueCount then () else
            Graph.flipEdge i g
            let delta = Graph.diffCliquesQuick cliqueSize i cliqueRecord g
            let newCliqueCount = cliqueCount + delta
            if newCliqueCount < lowestCliqueCount then
                lowestCliqueCount <- newCliqueCount
                bestEdge <- i
            Graph.flipEdge i g

        if bestEdge <> -1 then
            Graph.flipEdge bestEdge g
            Graph.diffCliquesFull cliqueSize bestEdge cliqueRecord g |> ignore
        lowestCliqueCount

    let makeFirstHelpful1Flip cliqueSize cliqueCount (cliqueRecord:CR) (g:G) =
        let mutable lowestCliqueCount = cliqueCount
        let mutable i1 = 0
        while i1 < g.Length do
            if cliqueCount - cliqueRecord.[i1] >= lowestCliqueCount then i1 <- i1 + 1 else
            Graph.flipEdge i1 g
            let delta = Graph.diffCliquesQuick cliqueSize i1 cliqueRecord g
            let newCliqueCount = cliqueCount + delta
            if newCliqueCount < lowestCliqueCount then
                Graph.diffCliquesFull cliqueSize i1 cliqueRecord g |> ignore
                lowestCliqueCount <- newCliqueCount
                i1 <- Int32.MaxValue
            else
                Graph.flipEdge i1 g
                i1 <- i1 + 1

        lowestCliqueCount


    let makeFirstHelpful2Flip cliqueSize cliqueCount (cliqueRecord:CR) (g:G) =
        let mutable lowestCliqueCount = cliqueCount
        let mutable i1 = 0
        let mutable i2 = 0

        let mutable go = true

        while i1 < g.Length - 1 && go do
            Graph.flipEdge i1 g
            let lvl1CliqueRecord = Array.copy cliqueRecord
            let lvl1DeltaCliques = Graph.diffCliquesFull cliqueSize i1 lvl1CliqueRecord g
            i2 <- i1 + 1
            while i2 < g.Length && go do
                if cliqueCount + lvl1DeltaCliques - lvl1CliqueRecord.[i2] >= lowestCliqueCount then i2 <- i2 + 1 else
                Graph.flipEdge i2 g
                let lvl2DeltaCliques = Graph.diffCliquesQuick cliqueSize i2 lvl1CliqueRecord g
                let newCliqueCount = cliqueCount + lvl1DeltaCliques + lvl2DeltaCliques
                if newCliqueCount < lowestCliqueCount then
                    Graph.diffCliquesFull cliqueSize i2 lvl1CliqueRecord g |> ignore
                    Array.Copy(lvl1CliqueRecord, cliqueRecord, lvl1CliqueRecord.Length)
                    lowestCliqueCount <- newCliqueCount
                    go <- false
                else
                    Graph.flipEdge i2 g
                    i2 <- i2 + 1
            if go then
                Graph.flipEdge i1 g
                i1 <- i1 + 1

        lowestCliqueCount

    let makeFirstHelpful3Flip cliqueSize cliqueCount (cliqueRecord:CR) (g:G) =
    //    printf "Checking 3" 
        let mutable lowestCliqueCount = cliqueCount
        let mutable i1 = 0
        let mutable i2 = 0
        let mutable i3 = 0

        let mutable go = true

        while i1 < g.Length - 2 && go do
     //       printf "."
            Graph.flipEdge i1 g
            let lvl1CliqueRecord = Array.copy cliqueRecord
            let lvl1DeltaCliques = Graph.diffCliquesFull cliqueSize i1 lvl1CliqueRecord g
            i2 <- i1 + 1

            while i2 < g.Length - 1 && go do
                Graph.flipEdge i2 g
                let lvl2CliqueRecord = Array.copy lvl1CliqueRecord
                let lvl2DeltaCliques = Graph.diffCliquesFull cliqueSize i2 lvl2CliqueRecord g
                i3 <- i2 + 1
                while i3 < g.Length && go do

                    if cliqueCount + lvl1DeltaCliques + lvl2DeltaCliques - lvl2CliqueRecord.[i3] >= lowestCliqueCount then i3 <- i3 + 1 else
                    Graph.flipEdge i3 g
                    let lvl3DeltaCliques = Graph.diffCliquesQuick cliqueSize i3 lvl2CliqueRecord g
                    let newCliqueCount = cliqueCount + lvl1DeltaCliques + lvl2DeltaCliques + lvl3DeltaCliques
                    if newCliqueCount < lowestCliqueCount then
                        Graph.diffCliquesFull cliqueSize i3 lvl2CliqueRecord g |> ignore
                        Array.Copy(lvl2CliqueRecord, cliqueRecord, lvl2CliqueRecord.Length)
                        lowestCliqueCount <- newCliqueCount
                        go <- false
                    else
                        Graph.flipEdge i3 g
                        i3 <- i3 + 1
                if go then
                    Graph.flipEdge i2 g
                    i2 <- i2 + 1
            if go then
                Graph.flipEdge i1 g
                i1 <- i1 + 1
    //    printfn ""
        lowestCliqueCount
    
    let rec private continuousImpl maxLevel level cliqueSize cliqueCount (cliqueRecord:CR) (g:G) =
        if level > maxLevel then cliqueCount else
        let newCliqueCount = 
            match level with
            | 1 -> makeFirstHelpful1Flip cliqueSize cliqueCount cliqueRecord g
            | 2 -> makeFirstHelpful2Flip cliqueSize cliqueCount cliqueRecord g
            | 3 -> makeFirstHelpful3Flip cliqueSize cliqueCount cliqueRecord g
            | _ -> cliqueCount

        if newCliqueCount < cliqueCount then
            let spacer = String.replicate (level * 6) " "
            printfn "New best (lvl %d):%s %d" level spacer newCliqueCount
            if newCliqueCount = 0 then
                printfn "%s" (Graph.toString Raw g)
                0
            else
                continuousImpl maxLevel 1 cliqueSize newCliqueCount  cliqueRecord g
        else
            if level = maxLevel then cliqueCount else
      //      printfn "Bumping level"
            continuousImpl maxLevel (level+1) cliqueSize cliqueCount cliqueRecord g

    let continuousScan maxScan cliqueSize (g:G) =
        let cliqueCount,cliqueRecord = Graph.numCliques_Record cliqueSize g
        continuousImpl maxScan 1 cliqueSize cliqueCount cliqueRecord g
