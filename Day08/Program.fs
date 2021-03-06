﻿open AoC2020.Common.Utilities.GameBoyComputer

// Advent of Code 2020 day 8.

[<EntryPoint>]
let main argv =

    let computer = GameBoyComputer(program = "InputFiles/Day08Input.txt")
    
    let rec runProgram mode =
        seq {                        
            let result = computer.Step()
            if result = Success then
                yield! runProgram mode
            elif result = mode then
                yield computer.Accumulator
            else
                yield 0
        }

    let tryProgram (flipAddress : int option) mode =
        computer.Reset()
        computer.FlipNopJmp(flipAddress)        
        let result = runProgram mode |> Seq.filter (fun r -> r > 0) |> Seq.sum
        computer.FlipNopJmp(flipAddress)
        result

    let result = tryProgram None Halt

    printfn "Part 1: result is %d" result
    
    let jmpResult = 
        computer.Memory |> Array.mapi (fun i m -> match m with 
                                                  | Jmp(offset) -> i
                                                  | _ -> -1 )
        |> Array.filter (fun j -> j >= 0)
        |> Array.map (fun j -> tryProgram (Some j) MemoryExceed)
        |> Array.sum
        
    let nopResult =
        computer.Memory |> Array.mapi (fun i m -> match m with 
                                                  | Nop(offset) -> i
                                                  | _ -> -1 )
        |> Array.filter (fun n -> n >= 0)
            |> Array.map (fun n -> tryProgram (Some n) MemoryExceed)    
        |> Array.sum

    printfn "Part 2: result is %d" (jmpResult + nopResult)
    0