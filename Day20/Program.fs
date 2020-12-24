open AoC2020.Common.Utilities
open AoC2020.Common.Utilities.ImageTile
open AoC2020.Common.Utilities.ImageJigsaw
open System

// Advent of Code 2020 day 20.

[<EntryPoint>]
let main argv =

    let tiles =
        "InputFiles/Day20Input.txt" 
        |> Seq.ofFileChunks ("\n\n")
        |> Seq.map (fun td ->
            let tokens = td.Split('\n')
            let tileId = tokens.[0].Split(' ').[1].Split(':').[0] |> int
            let imageGrid = tokens.[1..]
            { TileId = tileId; ImageGrid = imageGrid })
        |> Array.ofSeq

    // Create jigsaw.
    let sideLength = Math.Sqrt(float tiles.Length) |> int
    let jigsaw = ImageJigsaw.create sideLength sideLength tiles      
   
    // Get corners and multiply them.    
    let result =
        jigsaw
        |> ImageJigsaw.cornerTiles
        |> Array.map (fun t -> int64 t.TileId)
        |> Array.reduce (*)

    printfn "Part 1: result is %d" result   

    // Now look for nessie!!
    let countNessies (image : char array array) : int =

        let nessie = [|"                  # ";
                       "#    ##    ##    ###";
                       " #  #  #  #  #  #   "|]

        let getNessiePatch (image : char array array) rowOffset colOffset =
            [|for row in [rowOffset..rowOffset+2] do
                for col in [colOffset..colOffset+19] -> image.[row].[col]|]
            |> Array.chunkBySize 20
            |> Array.map (seq >> Seq.charSeqToString)

        let containsNessie' (nessie : string array) (patch : string array) : bool =
            [for row in [0..2] do
                for col in [0..19] -> (row,col)]
            |> List.map (fun (row,col) -> nessie.[row].[col] <> '#' || (nessie.[row].[col] = '#' && patch.[row].[col] = '#'))
            |> List.reduce (&&)
        let containsNessie = containsNessie' nessie

        let height = image.Length
        let width = image.[0].Length

        [|for row in [0..height-3] do
            for col in [0..width-20] ->
                containsNessie (getNessiePatch image row col)|]
        |> Array.filter (fun b -> b = true)
        |> Array.length

    // Place pieces.
    let jigsaw' =
        [for row in [0..sideLength-1] do
            for col in [0..sideLength-1] -> (row,col)]
        |> List.fold (fun acc (row,col) -> acc |> ImageJigsaw.placeTile row col) jigsaw

    // Get final image in a tile so it can be orientated.
    let image = jigsaw' |> ImageJigsaw.image
    let imageTile = ImageTile.createWholeImage image

    // Try all orientations of image and sum total count of nessies (this will be 0 where
    // orientation is wrong.)
    let noNessies =
        imageTile
        |> ImageTile.orientations
        |> Array.map (fun o -> o.getImageCharArray())
        |> Array.map (fun i -> countNessies i)
        |> Array.sum

    // Fnally, roughness is total number of '#' in final image - 15 x no. nessies.
    let result2 = imageTile.roughness() - 15 * noNessies
    printfn "Part 2: result is %A" result2
    0
