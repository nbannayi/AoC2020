open AoC2020.Common.Utilities

// Advent of Code 2020 day 6.
    
[<EntryPoint>]
let main argv =

    let forms = 
        "InputFiles/Day6Input.txt"
        |> Seq.ofFileChunks "\n\n"

    let totalNoDistinctAnswers =
        forms
        |> Seq.map (fun f -> f.Replace("\n",""))
        |> Seq.map (set)
        |> Seq.map (Set.count)
        |> Seq.sum
        
    let totalNoSharedAnswers =
        forms
        |> Seq.map (fun f -> f.Split('\n') |> Array.map (set))
        |> Seq.map (Set.intersectMany)
        |> Seq.map (Set.count)
        |> Seq.sum

    printf "Part 1: result is %d\n" totalNoDistinctAnswers    
    printf "Part 2: result is %d\n" totalNoSharedAnswers    
    0
