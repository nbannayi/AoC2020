namespace AoC2020.Common.Utilities

// Ferry module for AoC 2020 day 14.

module FerryComputer = 

    /// Modes to initalise memory.
    type InitMode =
        | Direct
        | AddressDecoder

    /// Create an AoC day 14 Ferry Computer.
    type FerryComputer(program : string, mode : InitMode) =

        // Internal State variables.
        // 36 bit mask.
        let mutable (mask: string) = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        let mutable (memory: Map<int64, int64>) = Map.empty

        let processAddressDecoder (value : int64) (mask : string) (address : int64) =

            let binaryAddress =
                address
                |> Maths.int64ToBinary 36
                             
            let address' =
                Seq.zip (binaryAddress |> seq) (mask |> seq)
                |> Seq.map (fun (a,m) -> match a,m with | a,'0' -> a | _,'1' -> '1' | _,'X' -> 'X' | _,_ -> a)
                |> Seq.charSeqToString                

            let noFloaters = address' |> Seq.filter (fun c -> c = 'X') |> Seq.length |> int

            let floaterRange = 
                let (maxRange: int64) = (Array.init noFloaters (fun _ -> 2L) |> Array.reduce (*)) - 1L

                seq {0L .. maxRange}
                |> Seq.map (fun f -> Maths.int64ToBinary noFloaters f)
                |> Seq.map (fun f -> (f |> seq) |> Seq.map (string))
                |> Array.ofSeq
                
            let maskArray = 
                let replace = address'.Replace("X",",X,").Replace(",,",",")
                let trimLeft = if replace.[0] = ',' then replace.[1..] else replace
                let trimRight = if trimLeft |> Seq.last = ',' then trimLeft.[0..(trimLeft.Length-2)] else trimLeft
                trimRight.Replace("X","").Split(',')
                
            let merge maskArray floaterSeq =                
                maskArray |> Array.fold (fun acc elem -> 
                    if elem <> "" 
                    then 
                        (fst acc, (snd acc) + elem) 
                    else
                        let i = (fst acc)
                        (i + 1, (snd acc) + (floaterSeq |> Seq.item i))) (0,"")
                |> snd

            floaterRange
            |> Array.map (merge maskArray)                
            |> Seq.map (Maths.binaryToInt64)
            |> Seq.map (fun a -> (a, value))
            |> Array.ofSeq
            |> Array.iter (fun m -> memory <- memory.Add(fst m, snd m))
                                                                            
        let processDirect (value : int64) (mask : string) (address : int64) =

            let binaryValue =
                value 
                |> Maths.int64ToBinary 36

            let value' = 
                Seq.zip (mask |> seq) (binaryValue |> seq)
                |> Seq.map (fun b -> match fst b, snd b with | '0',_ -> '0' | '1',_ -> '1' | 'X',b' -> b' | _,_ -> snd b)
                |> Seq.charSeqToString
                |> Maths.binaryToInt64                            

            memory <- memory.Add(address, value')           

        let processProgramLine (mode : InitMode) (line : string) = 
            let lineTokens = line.Split(" = ")
            let command = lineTokens.[0].[0..2]
            
            match command with
            | "mas" -> mask <- lineTokens.[1]
            | "mem" -> 
                let address = lineTokens.[0].[4..].Replace("]","") |> int64
                let value = lineTokens.[1] |> int64

                if mode = InitMode.Direct then
                    processDirect value mask address
                else
                    processAddressDecoder value mask address                       
            | _ -> failwith "Invalid command."
        
        // Constructor.
        do program
           |> Seq.ofFileLines
           |> Seq.iter (processProgramLine mode)
                
        /// Get memory of the computer.
        member this.Memory
            with get() = memory

        /// Get mask of the computer.
        member this.Mask
            with get() = mask
            
        /// Get memory at given address.
        member this.GetMemory(address : int64) =
            memory.[address]