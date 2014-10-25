namespace AMATHProj.Driver
open AMATHProj.Lib

module Program =

    [<EntryPoint>]
    let main _ =
        let g = Graph.init 43 2
        let e = g |> Graph.getEdge 2 7
        0