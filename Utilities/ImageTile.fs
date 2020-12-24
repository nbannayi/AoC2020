namespace AoC2020.Common.Utilities

// Image Tile module for AoC 2020 day 20.

module ImageTile =

    /// Flip modes for tile manipulation.
    type FlipMode =
        | Horizontal
        | Vertical

    /// Rotate modes for tile manipulation.
    type RotateMode =
        | Clockwise
        | Anticlockwise

    /// Edges of tiles.
    type EdgeType =
        | Top
        | Right
        | Bottom
        | Left
        | Unknown

    /// Create an AoC day 20 Image Tile.
    type ImageTile = 
        {
            /// Unqiue tile ID.
            TileId: int 
            /// Holds the image collected.
            ImageGrid: string array
        }

        /// Instatiate a tile for final whole image.
        static member createWholeImage (image : char array array) =
            { 
                TileId = 9999
                ImageGrid =
                    image
                    |> Array.map (seq >> Seq.charSeqToString)
            }

        member __.getImageCharArray() =
            __.ImageGrid
            |> Array.map (String.toCharSeq >> Seq.toArray)

        member __.roughness() =
            __.ImageGrid
            |> Array.map (fun g -> g |> String.toCharSeq |> Seq.filter(fun g' -> g' = '#') |> Seq.length)
            |> Array.sum

    /// Get specified edge. 
    let edge (edge : EdgeType) (tile : ImageTile) : string =        
        let edgeLength = tile.ImageGrid.[0].Length
        match edge with
        | Top -> tile.ImageGrid |> Array.head
        | Bottom -> tile.ImageGrid |> Array.last
        | Left -> tile.ImageGrid |> Array.map (fun g -> g.[0]) |> Seq.charSeqToString
        | Right -> tile.ImageGrid |> Array.map (fun g -> g.[edgeLength-1]) |> Seq.charSeqToString
        | Unknown -> "" // Something has gone wrong!
        
    /// Flip the tile along the given axis.
    let flip (mode : FlipMode) (tile : ImageTile) : ImageTile =

        let imageGrid' =
            if mode = Vertical then
                tile.ImageGrid |> Array.map (Seq.rev >> Seq.charSeqToString)
            else
                tile.ImageGrid |> Array.rev

        { TileId = tile.TileId; ImageGrid = imageGrid' }

    /// Rotate the tile 90 degress about the centre.
    let rotate (mode : RotateMode) (tile : ImageTile) : ImageTile =
   
        let transpose (tile : ImageTile) : ImageTile =

            let imageGrid' =
                let tileLength = tile.ImageGrid.[0].Length // Note: square tiles, length = width!
                [|for row in [0..tileLength-1] do
                    for col in [0..tileLength-1] do
                        yield tile.ImageGrid.[col].[row]|]
                |> Array.chunkBySize tileLength
                |> Array.map (seq >> Seq.charSeqToString)

            { TileId = tile.TileId; ImageGrid = imageGrid' }

        // To rotate clockwise flip horizontal then transpose.
        // To rotate anticlockwise flip vertical then transpose.
        if mode = Clockwise then
            tile |> (flip Horizontal >> transpose)
        else
            tile |> (flip Vertical >> transpose)

    /// Return an array of all possible tile orientations (flip and rotate.)
    let orientations (tile : ImageTile) : ImageTile array =
        let r = rotate Clockwise
        let f = flip Horizontal
        [|tile;
          tile |> r;
          tile |> (r >> r);
          tile |> (r >> r >> r);
          tile |> (r >> r >> r >> f);
          tile |> (r >> r >> r >> f >> r);
          tile |> (r >> r >> r >> f >> r >> r);
          tile |> (r >> r >> r >> f >> r >> r >> r)
        |]

    /// Given two tiles, and an edge, returns true if tile1 matches tile 2 on that edge of tile1.
    let matchEdge (tile2 : ImageTile) (edgeType : EdgeType) (tile1 : ImageTile) : bool =
        let tile1Edge = tile1 |> edge edgeType
        let tile2Edge = match edgeType with
                        | Left -> tile2 |> edge Right
                        | Right -> tile2 |> edge Left
                        | Top -> tile2 |> edge Bottom
                        | Bottom -> tile2 |> edge Top
                        | Unknown -> ""
        tile1Edge = tile2Edge

    /// Given two tiles return a tuple of the edge on which tile1 fits with tile2 from
    /// the perspective of tile1, and the translated tile2.
    let matchTile (tile2 : ImageTile) (tile1 : ImageTile) = 
        let tile2s = tile2 |> orientations

        let matched =
            tile2s
            |> Array.map (fun t2 ->
                let edges = [|Left;Right;Top;Bottom|]
                let matches = edges |> Array.map (fun e -> (e, matchEdge t2 e tile1))
                let result = matches |> Array.tryFind (fun (_,b) -> b = true)
                match result with
                | Some result -> (fst result, t2)
                | None -> (Unknown,  t2))
            |> Array.filter (fun t -> (fst t) <> Unknown)
            |> Array.tryHead

        match matched with
        | Some matched -> (fst matched |> Some, snd matched |> Some)            
        | None -> (None, None)            

    /// Crop the tile by removing the border
    let crop (tile : ImageTile) : ImageTile =
        let sideLength = tile.ImageGrid.Length
        
        let imageGrid' =
            tile.ImageGrid
            |> Array.map (fun g -> g.Substring(1, sideLength-2))
            |> Array.tail
            |> Array.take (sideLength-2)
            
        { tile with ImageGrid = imageGrid' }

    /// Display an image tile.
    let display (tile : ImageTile) =
        printfn "\nTile %d:" tile.TileId
        tile.ImageGrid |> Array.iter (printfn "%s")

    let empty = { TileId = 0; ImageGrid = [|""|]}
