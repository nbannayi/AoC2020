open AoC2020.Common.Utilities

// Advent of Code 2020 day 10.

[<EntryPoint>]
let main argv =

    let adapters =
        "InputFiles/Day10Input.txt" 
        |> Seq.ofFileLines
        |> Array.ofSeq
        |> Array.map (int64)

    let adapterJolts =
        let sortedAdapters = adapters |> Array.sort
        [| [|0L|]; sortedAdapters; [|(sortedAdapters |> Array.last) + 3L|] |] 
        |> Array.concat

    let groupedJolts = 
        adapterJolts
        |> Array.rev
        |> Array.windowed 2
        |> Array.map (fun j -> j.[0] - j.[1])
        |> Array.groupBy (fun j -> j)

    let joltsProduct =
        groupedJolts 
        |> Array.map (fun j -> (snd j) |> Array.length)
        |> Array.reduce (*)

    printf "Part 1: result is %d\n" joltsProduct

    let findPossibleAdapters (jolts : int64 array) : int64 =

        // Split into groups of [2], [3;2], [3;3;2] etc.
        (jolts
        |> Array.map (fun j -> 
            [|1L;2L;3L|]
            |> Array.map (fun j' -> if jolts |> Array.contains (j+j') then j+j' else 0L)
            |> Array.filter (fun j' -> j' <> 0L)
            |> Array.length)
        |> Array.map (fun i -> if i = 1 then " " else i.ToString() + ",")
        |> Array.reduce (+)).Split(' ')            
        |> Array.filter (fun s -> s <> "")
        |> Array.map (fun s -> 
            s.Split(',') 
            |> Array.map (fun s -> if s = "" || s = "0" then 0L else s |> int64) 
            |> Array.filter (fun i -> i <> 0L))
        |> Array.filter (fun a -> a.Length > 0)   
        // Did this part by exprimenting on paper - the counts that need to be multiplied are as below
        // In general this looks like the sequence 2,4,7,13,24, ..., sum of last 3 numbers
        // Only needed to go up to 3 for the input.
        |> Array.map (fun a -> match a.Length with
                               | 1 -> 2L
                               | 2 -> 4L
                               | 3 -> 7L
                               | _ -> 1L)
        |> Array.reduce (*)

    printf "Part 2: result is %d\n" (findPossibleAdapters adapterJolts)
    0
