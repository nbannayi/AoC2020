open AoC2020.Common.Utilities.Computer

// Advent of Code 2020 day 8.

[<EntryPoint>]
let main argv =

    let computer = Computer(program = "InputFiles/Day8Input.txt")

    let addressRepeated addresses =
        let addressesCount =
            addresses
            |> Array.countBy (fun a -> a)
            |> Array.filter (fun a -> snd a > 1)
            |> Array.length
        addressesCount > 0

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
    printf "Part 1: result is %d\n" result

    let jmpLocations =
        computer.Memory |> Array.mapi (fun i m -> match m with 
                                                  | Jmp(offset) -> i
                                                  | _ -> -1 )
        |> Array.filter (fun j -> j >= 0)

    let nopLocations =
        computer.Memory |> Array.mapi (fun i m -> match m with 
                                                  | Nop(offset) -> i
                                                  | _ -> -1 )
        |> Array.filter (fun n -> n >= 0)
        
    let jmpResult = 
        jmpLocations
        |> Array.map (fun j -> tryProgram (Some j) MemoryExceed)
        |> Array.sum
        
    let nopResult =
        nopLocations
        |> Array.map (fun n -> tryProgram (Some n) MemoryExceed)    
        |> Array.sum

    printf "Part 2: result is %d\n" (jmpResult + nopResult)
    0
