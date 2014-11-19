namespace AMATHProj.Lib

type LoopAction<'t> = Break | Continue | Proceed of 't

module ForLoop =
    let nested (nestLvl:int) (getRange : int -> int list -> (int*int)) (body : int -> int list -> 't -> LoopAction<_>) (initialData : 't)=
        let rec loop (currLvl : int) (startIndexStack:int list) (endIndexStack:int list) (earlierIndexes : int list) (state : 't) = 
            let startIndex :: startTail = startIndexStack
            let endIndex :: endTail = endIndexStack

            if startIndex > endIndex then
                match earlierIndexes with
                | [] -> ()
                | _::idxs -> loop (currLvl - 1) startTail endTail idxs state
            else

            let newIndexes = startIndex::earlierIndexes

            match body currLvl newIndexes state with
            | Break ->
                match earlierIndexes with
                | [] -> ()
                | _::idxs -> loop (currLvl - 1) startTail endTail idxs state
            | Continue -> loop currLvl ((startIndex+1)::startTail) endIndexStack earlierIndexes state
            | Proceed(newData) ->
                let newLevel = currLvl+1
                if newLevel = nestLvl then
                    loop currLvl ((startIndex+1)::startTail) endIndexStack earlierIndexes state
                else
                    let newStart, newEnd = getRange newLevel newIndexes
                    loop newLevel (newStart::(startIndex+1)::startTail) (newEnd::endIndexStack) newIndexes newData

        let startIndex, endIndex = getRange 0 []
        loop 0 [startIndex] [endIndex] [] initialData