open AoC2020.Common.Utilities
open AoC2020.Common.Utilities.HexTile
open AoC2020.Common.Utilities.HexFloor

// Advent of Code 2020 day 24.

[<EntryPoint>]
let main argv =

    let inputFile = "InputFiles/Day24Input.txt" 

    let tileLines =
        let parseTileLine (tileLine : string) =
            tileLine.Replace("nw","|nw|").Replace("ne","|ne|").Replace("se","|se|").Replace("sw","|sw|").Split("|")
            |> Array.map (fun s -> if not (s.Contains "n" || s.Contains "s") then s |> String.toStringSeq |> Array.ofSeq else [|s|])
            |> Array.collect (fun s -> s)
            |> Array.map (Direction.parse)
            
        inputFile
        |> Seq.ofFileLines
        |> Seq.map (parseTileLine)
        |> List.ofSeq

    let tiles =
        tileLines
        |> List.map (Array.fold (fun t direction -> t |> HexTile.move direction) (HexTile.create None))

    let floor = HexFloor.create tiles
    printfn "Part 1: result is %d" floor.NoBlackTiles

    // Runs quite slow in ~1 min but at this stage I'm just happy to have gotten this far :)
    // Try to optimise at some point...
    let f() =         
        let floor' =
            [1..100]
            |> List.fold (fun acc n -> HexFloor.rearrange acc) floor
        floor'.NoBlackTiles

    let result2 = Timer.duration "Part 2" f    
    printfn "Part 2: result is %d" result2
    0