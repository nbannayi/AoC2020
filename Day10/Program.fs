open AoC2020.Common.Utilities

// Advent of Code 2020 day 10.

[<EntryPoint>]
let main argv =

    let input =
        "InputFiles/Day10Input.txt" 
        |> Seq.ofFileLines

    printf "%A\n" (input |> Seq.head)
    printf "%A\n" (input |> Seq.last)
    0
