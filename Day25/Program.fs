open AoC2020.Common.Utilities

// Advent of Code 2020 day 25.

[<EntryPoint>]
let main argv =

    let publicKeys =
        "InputFiles/Day25Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (int)
        |> Array.ofSeq

    let cardKey = publicKeys.[0] |> int64
    let doorKey = publicKeys.[1] |> int64
   
    let getEncryptionKey key1 key2 =
        let transformSubjectNumber (subjectNumber : int64) (loopSize : int64) =
            Maths.modularExp subjectNumber loopSize 20201227L        

        let rec getLoopSize subjectNumber keyNumber loopNo =
            if (transformSubjectNumber subjectNumber loopNo) <> keyNumber then
                getLoopSize subjectNumber keyNumber (loopNo+1L)
            else
                loopNo

        let loopSize = getLoopSize 7L key1 1L
        transformSubjectNumber key2 loopSize

    printfn "Part 1: result is %A" (getEncryptionKey cardKey doorKey)
    printfn "Part 2: result is... there is no part 2 - hooray!"
    0