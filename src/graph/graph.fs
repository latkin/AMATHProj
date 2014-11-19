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

module Perturb =
    let recordClique edgeIndexes (cliqueRecord : CR) =
        let rec loop idxs =
            match idxs with
            | h :: t ->
                cliqueRecord.[h] <- cliqueRecord.[h] + 1
                loop t
            | [] -> ()
        loop edgeIndexes
            

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

    let getEdgesIdxsForVtxs vtxs = 
        let rec innerLoop currVtx otherVtxs result =
            match otherVtxs with
            | [] -> result
            | nextVtx :: tailVtx ->
                let e = getEdgeIndex currVtx nextVtx
                innerLoop currVtx tailVtx (e::result)
        let rec outerLoop remainingVtxs result =
            match remainingVtxs with
            | h :: t ->
                let newResult = innerLoop h t result
                outerLoop t newResult
            | _ -> result
        outerLoop vtxs []

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

    let rec private continuesClique newVtx prevVtxs clr g =
        match prevVtxs with
        | vtx :: vtxs ->
            let e = getEdge newVtx vtx g
            if e = clr then continuesClique newVtx vtxs clr g
            else false
        | [] -> true

    let rec private innerLoop func total startVtx endVtx level (prevVtxs:int list) color g =
        if startVtx > endVtx then total else
        let newTotal =
            match (continuesClique startVtx prevVtxs color g), level with
            | true, 1 ->
                match func with Some(f) -> f (startVtx :: prevVtxs) color | None -> ()
                total + 1
            | true, _ ->
                innerLoop func total (startVtx+1) (endVtx+1) (level-1) (startVtx::prevVtxs) color g
            | _ -> total            
        innerLoop func newTotal (startVtx+1) endVtx level prevVtxs color g

    let rec private secondLoop func total startVtx cliqueSize prevVtx gSize g =
        if startVtx > (gSize - cliqueSize + 1) then total else
        let (color, index) = getEdgeAndIndex startVtx prevVtx g
        let newTotal =
            innerLoop func total (startVtx+1) (gSize - cliqueSize + 2) (cliqueSize-2) [startVtx;prevVtx] color g
        secondLoop func newTotal (startVtx+1) cliqueSize prevVtx gSize g

    let rec private firstLoop func vtx cliqueSize gSize total g =
        if vtx > (gSize - cliqueSize) then total else
        let newTotal = secondLoop func total (vtx+1) cliqueSize vtx gSize g
        firstLoop func (vtx+1) cliqueSize gSize newTotal g        

    let numCliques cliqueSize (g:G) = 
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
                match continuesClique newVtx prevVtxs color g with
                | true ->
                    if lvl = (cliqueSize - 1) then
                        total <- total + 1
                    Descend
                | _ -> Continue

        ForLoop.nested cliqueSize getNextVtxRange loopBody
        total

    let numCliques_Record cliqueSize (g:G) =
        let gSize = size g
        let mutable total = 0
        let mutable color = -1
        let cliqueRecord : CR = init gSize 0

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
                match continuesClique newVtx prevVtxs color g with
                | true ->
                    if lvl = (cliqueSize - 1) then
                        total <- total + 1
                        let edgeIndexes = getEdgesIdxsForVtxs (newVtx::prevVtxs)
                        Perturb.recordClique edgeIndexes cliqueRecord
                    Descend
                | _ -> Continue

        ForLoop.nested cliqueSize getNextVtxRange loopBody
        (total, cliqueRecord)

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
