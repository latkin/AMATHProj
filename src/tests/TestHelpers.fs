namespace AMATHProj.Tests
open AMATHProj.Lib

module TestHelpers = 
    // dedicated, straightforward count of size-3 cliques
    let num3CliquesSimple gSize (g:G) =
        let mutable total = 0
        for i1 = 0 to gSize - 3 do
            for i2 = (i1+1) to gSize - 2 do
                let clr = Graph.getEdge i2 i1 g
                for i3 = (i2+1) to gSize - 1 do
                    if clr <> (Graph.getEdge i3 i1 g) then () else
                    if clr = (Graph.getEdge i3 i2 g) then
                        total <- total + 1
        total

    // dedicated, straightforward count of size-4 cliques
    let num4CliquesSimple gSize (g:G) =
        let mutable total = 0
        for i1 = 0 to gSize - 4 do
            for i2 = (i1+1) to gSize - 3 do
                let clr = Graph.getEdge i2 i1 g
                for i3 = (i2+1) to gSize - 2 do
                    if clr <> (Graph.getEdge i3 i2 g) then () else
                    if clr <> (Graph.getEdge i3 i1 g) then () else
                    for i4 = (i3+1) to gSize - 1 do
                        if clr <> (Graph.getEdge i4 i1 g) then () else
                        if clr <> (Graph.getEdge i4 i2 g) then () else
                        if clr = (Graph.getEdge i4 i3 g) then
                            total <- total + 1
        total

    // dedicated, straightforward count of size-5 cliques
    let num5CliquesSimple gSize (g:G) =
        let mutable total = 0
        for i1 = 0 to gSize - 5 do
            for i2 = (i1+1) to gSize - 4 do
                let clr = Graph.getEdge i2 i1 g
                for i3 = (i2+1) to gSize - 3 do
                    if clr <> (Graph.getEdge i3 i2 g) then () else
                    if clr <> (Graph.getEdge i3 i1 g) then () else
                    for i4 = (i3+1) to gSize - 2 do
                        if clr <> (Graph.getEdge i4 i1 g) then () else
                        if clr <> (Graph.getEdge i4 i2 g) then () else
                        if clr <> (Graph.getEdge i4 i3 g) then () else
                        for i5 = (i4+1) to gSize - 1 do
                            if clr <> (Graph.getEdge i5 i1 g) then () else
                            if clr <> (Graph.getEdge i5 i2 g) then () else
                            if clr <> (Graph.getEdge i5 i3 g) then () else
                            if clr = (Graph.getEdge i5 i4 g) then
                                total <- total + 1
        total