namespace AMATHProj.Lib

type LoopAction = Break | Continue | Return | Descend

module ForLoop =
    let nested (nestLvl:int) (getRange : int -> int list -> (int*int)) (body : int -> int list -> LoopAction) =
        let rec loop (currLvl : int) (startIndexStack:int list) (endIndexStack:int list) (earlierIndexes : int list)  = 
            let startIndex :: startTail = startIndexStack
            let endIndex :: endTail = endIndexStack

            if startIndex > endIndex then
                match earlierIndexes with
                | [] -> ()
                | _::idxs -> loop (currLvl - 1) startTail endTail idxs
            else

            let newIndexes = startIndex::earlierIndexes

            match body currLvl newIndexes with
            | Descend ->
                let newLevel = currLvl+1
                if newLevel = nestLvl then
                    loop currLvl ((startIndex+1)::startTail) endIndexStack earlierIndexes
                else
                    let newStart, newEnd = getRange newLevel newIndexes
                    loop newLevel (newStart::(startIndex+1)::startTail) (newEnd::endIndexStack) newIndexes
            | Break ->
                match earlierIndexes with
                | [] -> ()
                | _::idxs -> loop (currLvl - 1) startTail endTail idxs
            | Continue -> loop currLvl ((startIndex+1)::startTail) endIndexStack earlierIndexes
            | Return -> ()

        let startIndex, endIndex = getRange 0 []
        loop 0 [startIndex] [endIndex] []