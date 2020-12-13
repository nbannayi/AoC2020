open AoC2020.Common.Utilities

// Advent of Code 2020 day 13.

open System

[<EntryPoint>]
let main argv =

    let timetable =
        "InputFiles/Day13Input.txt" 
        |> Seq.ofFileLines
        |> Array.ofSeq

    let timestamp = timetable.[0] |> float

    let busTimes = 
        timetable.[1].Split(',')
        |> Array.map (fun t -> if t = "x" then 0. else float t)

    // Array of (wait time, bus id).
    let busWaitTimes =
        busTimes
        |> Array.filter (fun t -> t <> 0.)
        |> Array.map (fun t -> (Math.Ceiling(timestamp/t) * t - timestamp, t))
        |> Array.sort       

    let earliestBus = busWaitTimes |> Array.head

    printfn "Part 1: result is %d" ((fst earliestBus * snd earliestBus) |> int)

    let n = busTimes |> Array.length |> int64
        
    let mods = 
        busTimes 
        |> Array.mapi (fun i t -> (float n - float i, t))
        |> Array.filter (fun m -> snd m <> 0.0)
        |> Array.map (fst >> int64)
        
    let busTimes' =
        busTimes
        |> Array.mapi (fun i t -> (float i, t))
        |> Array.filter (fun m -> snd m <> 0.0)
        |> Array.map (snd >> int64)

    let result = Maths.ChineseRemainder mods busTimes'
    printf "Part 2: result is %A" (match result with 
                                   | Some result -> result - n
                                   | _ -> 0L)
    0