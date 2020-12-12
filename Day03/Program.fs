open AoC2020.Common.Utilities

open System

// Advent of Code 2020 day 3.

[<EntryPoint>]
let main argv =

    // Create whole forest based on puzzle input 323 long x 31 wide.
    let makeForest x y =
        let totalWidthRequired = Math.Ceiling(323. * (double x) / (double y)) |> int
        let widthRequired = Math.Ceiling((double totalWidthRequired) / 31.) |> int

        "InputFiles/Day03Input.txt" 
        |> Seq.ofFileLines
        |> Array.ofSeq
        |> Array.map (fun l -> [|for _ in 1..widthRequired -> l|] |> Array.reduce (+))

    let countTrees x y : int64 =    
        let forest = makeForest x y
        let forestWidth = forest.[0].Length

        Seq.zip [for i in [|0..x..forestWidth-1|] -> i] 
                [for j in [|0..y..forest.Length-1|] -> j]
        |> Seq.map (fun (x,y) -> forest.[y].[x])
        |> Seq.filter (fun p -> p = '#')
        |> Seq.length  
        |> int64

    printfn "Part 1: result is %d" (countTrees 3 1)

    let totalTrees = 
        [|(1,1);(3,1);(5,1);(7,1);(1,2)|] 
        |> Array.map (fun (x,y) -> countTrees x y) 
        |> Array.reduce (*) 

    printfn "Part 2: result is %d" totalTrees
    0