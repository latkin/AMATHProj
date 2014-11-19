namespace AMATHProj.Lib

// How graphs are represented here
//
// full adjacency matrix for n-vertex graph is size n x n, however 
// since it's symmetric and the main diagoal doesn't matter, we only need to store
// "half", for a total of n*(n-1)/2 elements. Example (n = 4):
// instead of  x 1 0 1  we just take         and store as [| 1  0 0  1 1 0 |]
//             1 x 0 1                1 
//             0 0 x 0                0 0 
//             1 1 0 x                1 1 0 
//
//  this saves space, but makes it trickier to index in to obtain the edge between vertex
//  v_i and v_j.
//
//  If i > j, edges in row i (zero-based) begin at array index i(i-1)/2
//    then we move over j columns, so final index is (i^2 - i)/2 + j.
//  If i < j, we just swap the roles of i and j in the above expression.

// graph
type G = int array
// clique record
type CR = int array

type Format = Raw | Pretty

module Graph =
    open System
    open System.Text

    let private rnd = Random()

    let init nVtx clr : G =
        Array.create ((nVtx * (nVtx - 1)) / 2) clr

    let random nVtx nClr : G =
        Array.init ((nVtx * (nVtx - 1)) / 2) (fun _ -> rnd.Next() % nClr)

    let parse (s:string) : G =
        Array.init s.Length (fun i -> Int32.Parse(s.[i].ToString()))

    let inline getEdge i j (g:G) =
        if i > j then g.[(i*i - i)/2 + j] else
        g.[(j*j - j)/2 + i]

    let inline getEdgeAndIndex i j (g:G) =
        let index = if i > j then (i*i - i)/2 + j else (j*j - j)/2 + i
        (g.[index], index)

    let inline getEdgeIndex i j =
        if i > j then (i*i - i)/2 + j else (j*j - j)/2 + i

    let inline setEdge i j clr (g:G) =
        if i > j then g.[(i*i - i)/2 + j] <- clr else
        g.[(j*j - j)/2 + i] <- clr
    
    let inline size (g:G) = (g.Length * 2) |> float |> sqrt |> ceil |> int

    let toString f (g:G) =
        let sb = StringBuilder()
        match f with
        | Raw -> for e in g do ignore (sb.Append(e))
        | Pretty ->
            let n = 1 + (g.Length*2) |> float |> sqrt |> int
            for i = 0 to n do
                for j = 0 to n do
                    if i = j then ignore (sb.Append("x ")) else
                    ignore (sb.Append(getEdge i j g))
                    ignore (sb.Append(' '))
                ignore (sb.Remove(sb.Length - 1, 1))
                if i <> n then ignore (sb.Append(Environment.NewLine))
        sb.ToString()

    let inline flipEdge index (g:G) =
        let currValue = g.[index]
        if currValue = 0 then g.[index] <- 1
        else g.[index] <- 0

    let inline flip i j (g:G) =
        let index = getEdgeIndex i j
        flipEdge index g

    module Dedicated =
        let num3Cliques (g:G) =
            let gSize = size g
            let mutable total = 0
            for i1 = 0 to gSize - 3 do
                for i2 = (i1+1) to gSize - 2 do
                    let clr = getEdge i2 i1 g
                    for i3 = (i2+1) to gSize - 1 do
                        if clr <> (getEdge i3 i1 g) then () else
                        if clr = (getEdge i3 i2 g) then
                            total <- total + 1
            total

        let num4Cliques (g:G) =
            let gSize = size g
            let mutable total = 0
            for i1 = 0 to gSize - 4 do
                for i2 = (i1+1) to gSize - 3 do
                    let clr = getEdge i2 i1 g
                    for i3 = (i2+1) to gSize - 2 do
                        if clr <> (getEdge i3 i2 g) then () else
                        if clr <> (getEdge i3 i1 g) then () else
                        for i4 = (i3+1) to gSize - 1 do
                            if clr <> (getEdge i4 i1 g) then () else
                            if clr <> (getEdge i4 i2 g) then () else
                            if clr = (getEdge i4 i3 g) then
                                total <- total + 1
            total

        let num5Cliques (g:G) =
            let gSize = size g
            let mutable total = 0
            for i1 = 0 to gSize - 5 do
                for i2 = (i1+1) to gSize - 4 do
                    let clr = getEdge i2 i1 g
                    for i3 = (i2+1) to gSize - 3 do
                        if clr <> (getEdge i3 i2 g) then () else
                        if clr <> (getEdge i3 i1 g) then () else
                        for i4 = (i3+1) to gSize - 2 do
                            if clr <> (getEdge i4 i1 g) then () else
                            if clr <> (getEdge i4 i2 g) then () else
                            if clr <> (getEdge i4 i3 g) then () else
                            for i5 = (i4+1) to gSize - 1 do
                                if clr <> (getEdge i5 i1 g) then () else
                                if clr <> (getEdge i5 i2 g) then () else
                                if clr <> (getEdge i5 i3 g) then () else
                                if clr = (getEdge i5 i4 g) then
                                    total <- total + 1
            total

    let rec private continuesClique newVtx prevVtxs prevIdxs clr g =
        match prevVtxs with
        | vtx :: vtxs ->
            let (e, i) = getEdgeAndIndex newVtx vtx g
            if e = clr then continuesClique newVtx vtxs (i::prevIdxs) clr g
            else (false, prevIdxs)
        | [] -> (true, prevIdxs)

    let rec private innerLoop func total startVtx endVtx level (prevVtxs:int list) (prevIdxs:int list) color g =
        if startVtx > endVtx then total else
        let newTotal =
            match (continuesClique startVtx prevVtxs prevIdxs color g), level with
            | (true, indexes), 1 ->
                match func with Some(f) -> f (startVtx :: prevVtxs) indexes color | None -> ()
                total + 1
            | (true, indexes), _ ->
                innerLoop func total (startVtx+1) (endVtx+1) (level-1) (startVtx::prevVtxs) indexes color g
            | _ -> total            
        innerLoop func newTotal (startVtx+1) endVtx level prevVtxs prevIdxs color g

    let rec private secondLoop func total startVtx cliqueSize prevVtx gSize g =
        if startVtx > (gSize - cliqueSize + 1) then total else
        let (color, index) = getEdgeAndIndex startVtx prevVtx g
        let newTotal =
            innerLoop func total (startVtx+1) (gSize - cliqueSize + 2) (cliqueSize-2) [startVtx;prevVtx] [index] color g
        secondLoop func newTotal (startVtx+1) cliqueSize prevVtx gSize g

    let rec private firstLoop func vtx cliqueSize gSize total g =
        if vtx > (gSize - cliqueSize) then total else
        let newTotal = secondLoop func total (vtx+1) cliqueSize vtx gSize g
        firstLoop func (vtx+1) cliqueSize gSize newTotal g        

    let numCliques cliqueSize (g:G) = 
        match cliqueSize with
        | s when s < 3 -> failwith "specified clique size is too small"
        | 3 -> Dedicated.num3Cliques g
        | 4 -> Dedicated.num4Cliques g
        | 5 -> Dedicated.num5Cliques g
        | _ ->
            let gSize = size g
            firstLoop None 0 cliqueSize gSize 0 g

    let numCliquesNoDedicated cliqueSize (g:G) = 
        let gSize = size g
        firstLoop None 0 cliqueSize gSize 0 g

    let numCliquesFunctionalFor cliqueSize (g:G) =
        let gSize = size g
        let mutable total = 0
        let mutable color = -1

        let getNextVtxRange lvl prevVtx =
            if lvl = 0 then 0, (gSize - cliqueSize) else
            (prevVtx+1), (gSize - cliqueSize + lvl)

        let loopBody (lvl:int) (vtxs:int list) =
            match lvl with
            | 0 -> Descend
            | 1 ->
                let [v1;v2] = vtxs
                color <- getEdge v1 v2 g
                Descend
            | _ ->
                let newVtx::prevVtxs = vtxs
                match continuesClique newVtx prevVtxs [] color g with
                | true, _ ->
                    if lvl = (cliqueSize - 1) then
                        total <- total + 1
                    Descend
                | _ -> Continue

        ForLoop.nested cliqueSize getNextVtxRange loopBody
        total

    let numCliques_Record cliqueSize (g:G) =
        let gSize = size g
        let cliqueRecord : CR = init gSize 0
        let onCliqueFound _ idxs _ =
            for i in idxs do
                cliqueRecord.[i] <- cliqueRecord.[i] + 1
        let numCliques = firstLoop (Some(onCliqueFound)) 0 cliqueSize gSize 0 g
        (numCliques, cliqueRecord)

    module Parallel =
        open System.Threading.Tasks

        let numCliques cliqueSize (g:G) =
            let gSize = size g
            let results = Array.zeroCreate (gSize - cliqueSize + 1)
            results
            |> Array.mapi (fun i _ -> Task.Run(fun () ->
                    results.[i] <- secondLoop None 0 (i+1) cliqueSize i gSize g))
            |> Task.WaitAll
            results |> Array.sum
