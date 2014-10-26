namespace AMATHProj.Tests
open AMATHProj.Lib

module TestHelpers = 
    // dedicated, straightforward count of size-3 cliques
    let num3CliquesSimple gSize (g:G) =
        let mutable total = 0
        for i1 = 0 to gSize - 3 do
            for i2 = (i1+1) to gSize - 2 do
                let e1 = Graph.getEdge i2 i1 g
                for i3 = (i2+1) to gSize - 1 do
                    let e2 = Graph.getEdge i3 i1 g
                    if e1<>e2 then () else
                    let e3 = Graph.getEdge i3 i2 g
                    if e2=e3 then
                        total <- total + 1
        total

    // dedicated, straightforward count of size-4 cliques
    let num4CliquesSimple gSize (g:G) =
        let mutable total = 0
        for i1 = 0 to gSize - 4 do
            for i2 = (i1+1) to gSize - 3 do
                let e1 = Graph.getEdge i2 i1 g
                for i3 = (i2+1) to gSize - 2 do
                    let e2 = Graph.getEdge i3 i2 g
                    if e2<>e1 then () else
                    let e3 = Graph.getEdge i3 i1 g
                    if e2<>e3 then () else
                    for i4 = (i3+1) to gSize - 1 do
                        let e4 = Graph.getEdge i4 i1 g
                        if e3<>e4 then () else
                        let e5 = Graph.getEdge i4 i2 g
                        if e4<>e5 then () else
                        let e6 = Graph.getEdge i4 i3 g
                        if e5=e6 then
                            total <- total + 1
        total