namespace AMATHProj.Tests
open Xunit
open AMATHProj.Lib
open System

type Tests() =

    [<Fact>]
    member __.Init() =
        let color = 2
        let g = Graph.init 20 color
        for e in g do Assert.True((e = color))

    [<Fact>]
    member __.Random() =
        let colors = 5
        let g = Graph.random 20 colors
        for e in g do Assert.True((e >= 0 && e <= colors))

    [<Fact>]
    member __.Parse() =
        let g = Graph.random 50 5
        let s = g |> Graph.toString Raw
        let g2 = s |> Graph.parse
        g2 = g |> Assert.True

    [<Fact>]
    member __.ToString() =
        (*
           x 2 1 2
           2 x 2 0
           1 2 x 2
           2 0 2 x
        *)
        let g = Graph.init 4 2
        g |> Graph.setEdge 1 3 0
        g |> Graph.setEdge 2 0 1

        let s = g |> Graph.toString Raw
        s = "212202" |> Assert.True

        let sP = g |> Graph.toString Pretty
        let (++) (s:string) (s2:string) = String.Concat(s, s2)
        let expected =
            "x 2 1 2" ++ Environment.NewLine ++
            "2 x 2 0" ++ Environment.NewLine ++
            "1 2 x 2" ++ Environment.NewLine ++
            "2 0 2 x"
        printfn "expected: %s" expected
        printfn "actual: %s" sP
        sP = expected |> Assert.True

    [<Fact>]
    member __.GetEdge() =
        let g = Graph.parse "1234567890"
        let testIJ i j clr =
            clr = (Graph.getEdge i j g) |> Assert.True
            clr = (Graph.getEdge j i g) |> Assert.True
        testIJ 1 0 1
        testIJ 2 0 2
        testIJ 2 1 3
        testIJ 3 0 4
        testIJ 3 1 5
        testIJ 3 2 6
        testIJ 4 0 7
        testIJ 4 1 8
        testIJ 4 2 9
        testIJ 4 3 0

    [<Fact>]
    member __.SetEdge() =
        let g = Graph.init 5 0
        g |> Graph.setEdge 1 0 1
        g |> Graph.setEdge 2 0 2
        g |> Graph.setEdge 2 1 3
        g |> Graph.setEdge 3 0 4
        g |> Graph.setEdge 3 1 5
        g |> Graph.setEdge 3 2 6
        g |> Graph.setEdge 4 0 7
        g |> Graph.setEdge 4 1 8
        g |> Graph.setEdge 4 2 9
        g |> Graph.setEdge 4 3 0
        let s = g |> Graph.toString Raw
        "1234567890" = s |> Assert.True

    [<Fact>]
    member __.NumCliques() =
        let g = Graph.init 3 0
        let num = Graph.numCliques 3 3 g
        Assert.Equal(1, num)

        let g = Graph.init 4 0
        let num = Graph.numCliques 3 4 g
        Assert.Equal(4, num)

        let g = Graph.init 10 1
        let num = Graph.numCliques 4 10 g
        Assert.Equal(210, num)

        g |> Graph.setEdge 0 1 0
        let num = Graph.numCliques 4 10 g
        Assert.Equal(182, num)

        g |> Graph.setEdge 0 2 0
        g |> Graph.setEdge 1 2 0
        let num = Graph.numCliques 3 10 g
        Assert.Equal(99, num)

        let g = Graph.init 43 0
        let num = Graph.numCliques 5 43 g
        Assert.Equal(962598, num)

        let g = Graph.init 30 0
        let num = Graph.numCliques 8 30 g
        Assert.Equal(5852925, num)

        for __ = 1 to 10 do
            let g = Graph.random 40 3
            let expected3 = TestHelpers.num3CliquesSimple 40 g
            let actual3 = Graph.numCliques 3 40 g
            Assert.Equal(expected3, actual3)
            let expected4 = TestHelpers.num4CliquesSimple 40 g
            let actual4 = Graph.numCliques 4 40 g
            Assert.Equal(expected4, actual4)
