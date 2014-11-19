namespace AMATHProj.Lib

type LoopAction<'t> = Break | Continue | Proceed of 't

module ForLoop =
    let nested (level:int) (nextRange : int -> int list -> (int*int)) (levelBody : int -> int list -> 't -> LoopAction<_>) (initialData : 't)=
        let rec loop (lvl : int) (indexes : int list) (data : 't) = 
            if lvl = level then () else
            let startIndex, endIndex = nextRange lvl indexes
            let mutable i = startIndex
            while i <= endIndex do
                let idxs = i::indexes
                match levelBody lvl idxs data with
                | Break -> i <- endIndex
                | Continue -> ()
                | Proceed(newData) -> loop (lvl+1) idxs newData
                i <- i + 1
        loop 0 [] initialData