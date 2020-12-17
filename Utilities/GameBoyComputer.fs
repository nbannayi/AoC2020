namespace AoC2020.Common.Utilities

// Computer class for AoC 2020 day 8.

module GameBoyComputer = 

    /// Represents a CPU instruction.
    type Instruction =
        | Nop of offset : int
        | Acc of offset : int
        | Jmp of offset : int

    /// Status modes after step.
    type Status =
        | Halt
        | MemoryExceed
        | Success
            
    /// Create a AoC Day 8 computer.
    type GameBoyComputer(program : string) =

        let parseInstruction (programLine : string) =
            let opcode = programLine.Split(' ').[0]
            let offset = programLine.Split(' ').[1] |> int

            match opcode with
            | "nop" -> Nop(offset)
            | "acc" -> Acc(offset)
            | "jmp" -> Jmp(offset)
            | _ -> failwith "Invalid operation."

        let mutable memory =
            program
            |> Seq.ofFileLines
            |> Seq.map (parseInstruction)
            |> Array.ofSeq

        // Internal State variables.
        let mutable accumulator = 0
        let mutable instructionPointer = 0
        let mutable addressesProcessed = [||]

        /// Get current memory address being processed.
        member __.InstructionPointer
            with get() = instructionPointer

        /// Get current value of accumulator register.
        member __.Accumulator
            with get() = accumulator

        /// Get full memory.
        member __.Memory
            with get() = memory

        /// Get current instruction being processed.
        member __.CurrentInstruction
            with get() = memory.[instructionPointer]

        /// Flip nop to jmp or vice versa.
        member __.FlipNopJmp(address : int option) =
            match address with
            | Some(address) -> memory.[address] <- match memory.[address] with
                                                   | Nop(offset) -> Jmp(offset)
                                                   | Jmp(offset) -> Nop(offset)
                                                   | Acc(offset) -> Acc(offset)
            | None -> () 

        /// Reset all registers.
        member __.Reset() = 
            accumulator <- 0
            instructionPointer <- 0
            addressesProcessed <- [||]

        /// Step through current instruction returning status.
        member __.Step() =

            // Exceeded availble memory, or stuck in a loop - exit.
            if instructionPointer = memory.Length then
                MemoryExceed
            elif (addressesProcessed
                 |> Array.countBy (fun a -> a)
                 |> Array.filter (fun a -> snd a > 1)
                 |> Array.length > 1) then
                Halt
            else // Success!!            
                match memory.[instructionPointer] with
                | Nop(offset) -> instructionPointer <- instructionPointer + 1
                | Acc(offset) -> accumulator <- accumulator + offset
                                 instructionPointer <- instructionPointer + 1
                | Jmp(offset) -> instructionPointer <- instructionPointer + offset

                addressesProcessed <- addressesProcessed |> Array.append [|instructionPointer|]
                Success
