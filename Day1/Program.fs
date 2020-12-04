open AoC2020.Common.Utilities

// Advent of Code 2020 day 1.

[<EntryPoint>]
let main argv =
    
    let input = "InputFiles/Day1Input.txt" |> Seq.ofFileLines
    let expenses = input |> Seq.map (int) |> Array.ofSeq
    
    let e1, e2 =
        [for i in 0..expenses.Length-1 do
            for j in 0..expenses.Length-1 -> if i <> j then (i,j) else (-1,-1)]
        |> List.filter (fun (a,b) -> a > 0 && expenses.[a] + expenses.[b] = 2020)
        |> List.head

    printf "Part 1: result is %d\n" (expenses.[e1] * expenses.[e2]) 
   
    let e1, e2, e3 =
        [for i in 0..expenses.Length-1 do
            for j in 0..expenses.Length-1 do
                for k in 0..expenses.Length-1 -> if i <> j && j <> k then (i,j,k) else (-1,-1,-1)]
        |> List.filter (fun (a,b,c) -> a > 0 && expenses.[a] + expenses.[b] + expenses.[c] = 2020)
        |> List.head

    printf "Part 2: result is %d\n" (expenses.[e1] * expenses.[e2] * expenses.[e3]) 
    0 
