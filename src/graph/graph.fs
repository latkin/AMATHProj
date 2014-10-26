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

type G = int array

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

    let inline setEdge i j clr (g:G) =
        if i > j then g.[(i*i - i)/2 + j] <- clr else
        g.[(j*j - j)/2 + i] <- clr

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

    let rec private continuesClique newVtx prevVtxs clr g =
        match prevVtxs with
        | vtx :: vtxs ->
            let e = getEdge newVtx vtx g
            if e = clr then continuesClique newVtx vtxs clr g
            else false
        | [] -> true

    let rec private innerLoop total startVtx endVtx level (prevVtxs:int list) color gSize g =
        if startVtx > endVtx then total else
        let newTotal =
            match (continuesClique startVtx prevVtxs color g), level with
            | true, 1 -> total + 1
            | true, _ -> innerLoop total (startVtx+1) (endVtx+1) (level-1) (startVtx::prevVtxs) color gSize g
            | _ -> total            
        innerLoop newTotal (startVtx+1) (endVtx) (level) (prevVtxs) color gSize g

    let rec private secondLoop total startVtx cliqueSize prevVtx gSize g =
        if startVtx > (gSize - cliqueSize + 1) then total else
        let color = getEdge startVtx prevVtx g
        let newTotal =
            innerLoop total (startVtx+1) (gSize - cliqueSize + 2) (cliqueSize-2) [startVtx;prevVtx] color gSize g
        secondLoop newTotal (startVtx+1) cliqueSize prevVtx gSize g

    let rec private firstLoop vtx cliqueSize gSize total g =
        if vtx > (gSize - cliqueSize) then total else
        let newTotal = secondLoop total (vtx+1) cliqueSize vtx gSize g
        firstLoop (vtx+1) cliqueSize gSize newTotal g        

    let numCliques cliqueSize gSize (g:G) =
        firstLoop 0 cliqueSize gSize 0 g

                        
                    
