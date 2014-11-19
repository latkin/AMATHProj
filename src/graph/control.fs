namespace AMATHProj.Lib
open System.Collections.Generic

type LoopAction = Break | Continue | Return | Descend

module ForLoop =
    let nested (nestLvl:int) (getRange : int -> int -> (int*int)) (body : int -> int list -> LoopAction) =
        let rec loop (currLvl : int) (startIndexStack:int list) (endIndexStack:int list) (earlierIndexes : int list)  = 
            let startIndex :: startTail = startIndexStack
            let endIndex :: endTail = endIndexStack

            if startIndex > endIndex then
                if currLvl = 0 then () else
                let _::idxTail = earlierIndexes
                loop (currLvl - 1) startTail endTail idxTail
            else

            let newIndexes = startIndex::earlierIndexes
            let newLevel = currLvl+1

            match body currLvl newIndexes with
            | Descend when newLevel < nestLvl ->
                let newStart, newEnd = getRange newLevel startIndex
                loop newLevel (newStart::(startIndex+1)::startTail) (newEnd::endIndexStack) newIndexes
            | Break ->
                if currLvl = 0 then () else
                let _::idxTail = earlierIndexes
                loop (currLvl - 1) startTail endTail idxTail
            | Return -> ()
            | _ -> loop currLvl ((startIndex+1)::startTail) endIndexStack earlierIndexes

        let startIndex, endIndex = getRange 0 -1
        loop 0 [startIndex] [endIndex] []

