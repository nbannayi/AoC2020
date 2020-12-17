open AoC2020.Common.Utilities.FerryComputer

// Advent of Code 2020 day 14.

[<EntryPoint>]
let main argv =
    
    let computer1 = FerryComputer(program = "InputFiles/Day14Input.txt", mode = InitMode.Direct)
    let computer2 = FerryComputer(program = "InputFiles/Day14Input.txt", mode = InitMode.AddressDecoder)

    let result1 = computer1.Memory |> Map.toArray |> Array.sumBy (snd >> int64)
    printfn "Part 1: result is %d" result1

    let result2 = computer2.Memory |> Map.toArray |> Array.sumBy (snd >> int64)
    printfn "Part 2: result is %d" result2
    0 
