open AoC2020.Common.Utilities

// Advent of Code 2020 day 9.

[<EntryPoint>]
let main argv =

    let xmas =
        "InputFiles/Day09Input.txt" 
        |> Seq.ofFileLines
        |> Seq.map (int64)
        |> Array.ofSeq

    // Update this for different example.
    let preambleSize = 25    
    
    let allSums (xmas : int64 array) = 
        [|for i in 0..xmas.Length-1 do
            for j in 0..xmas.Length-1 -> if i <> j then (i,j) else (-1,-1)|]
        |> Array.filter (fun x -> fst x <> -1)
        |> Array.map (fun x -> xmas.[fst x] + xmas.[snd x])
        |> Array.distinct
                        
    let result =
        let preamble (xmas : int64 array) start size =
            [|for i in start..start+size-1 -> xmas.[i]|]

        let next (xmas : int64 array) start size = 
            xmas.[start+size]            

        let check (xmas : int64 array) checkNum =
            (allSums xmas) |> Array.contains checkNum

        [|0..xmas.Length-preambleSize-1|]
        |> Array.map (fun w -> 
            let nextCode = next xmas w preambleSize
            let preambleCodes = preamble xmas w preambleSize
            (nextCode, check preambleCodes nextCode))
        |> Array.find (fun w -> (snd w) = false)
        |> fst

    printfn "Part 1: result is %d" result

    // Work out the point where the sum is exceeded.
    let maxRange = 
        seq { 0 .. xmas.Length-1 } 
        |> Seq.find (fun i -> 
            ([|0..i|] 
            |> Array.map (fun a -> xmas.[a]) 
            |> Array.sum) > result)

    // Create an array of windows to now check from maxRange down.
    let windows =
        [|maxRange..xmas.Length-1|]
        |> Array.map (fun w -> [|w..(-1)..0|])        

    // Check each window.
    let processWindow (window : int array) =

        let location = 
            seq { 0 .. window.Length-1 }
            |> Seq.tryFind (fun i -> 
                ([|0..i|]
                 |> Array.map (fun a -> xmas.[window.[a]]) 
                 |> Array.sum) = result)
            
        match location with
        | Some(location) -> 
            if window.[location] = window.[0] then
                0L
            else            
                let requiredRange = 
                    [|window.[location]..window.[0]|]
                    |> Array.map (fun w -> xmas.[w])                
                (requiredRange |> Array.min) + (requiredRange |> Array.max)
        | _ -> 0L       

    let result2 = 
        windows
        |> Array.map (processWindow)
        |> Array.sum

    printfn "Part 2: result is %d" result2
    0