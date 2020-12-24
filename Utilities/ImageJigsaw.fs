namespace AoC2020.Common.Utilities

open AoC2020.Common.Utilities.ImageTile

// Image Jigsaw module for AoC 2020 day 20.

module ImageJigsaw =

    type PositionType =
        | TopLeftCorner
        | LeftEdge
        | BottomLeftCorner
        | TopEdge
        | Middle
        | BottomEdge
        | TopRightCorner
        | RightEdge
        | BottomRightCorner

    /// Create an AoC day 20 Image Board.
    type ImageJigsaw = 
        {
            /// Width of puzzle board.
            BoardWidth: int
            /// Hight of puzzle board.
            BoardHeight: int
            /// All the puzzle pieces to fit.
            ImageTiles: ImageTile array 
            /// Holds the placed pieces.
            BoardGrid: ImageTile array array
        }
        with 

        /// Instatiate an Image Jigsaw type.
        static member create width height tiles =
            { 
                BoardWidth = width
                BoardHeight = height
                ImageTiles = tiles
                BoardGrid = Array.init height (fun _ -> Array.init width (fun _ -> ImageTile.empty))
            }

    /// Get type of tile needed for a specific position on the board.
    let positionType (row : int) (col : int) (board : ImageJigsaw) : PositionType =
        let height = board.BoardHeight-1
        let width = board.BoardWidth-1

        match row, col with
        | row, col when row = 0 && col = 0 -> TopLeftCorner
        | row, col when row = height && col = 0 -> BottomLeftCorner
        | row, col when row = height && col = width -> BottomRightCorner        
        | row, col when row = 0 && col = width -> TopRightCorner
        | row, col when row = 0 && col > 0 && col < width -> TopEdge
        | row, col when row > 0 && row < height && col = 0 -> LeftEdge
        | row, col when row = height && col > 0 && col < width -> BottomEdge
        | row, col when row > 0 && row < height && col = width -> RightEdge
        | row, col when row > 0 && row < height && col > 0 && col < width -> Middle        
        | _, _ -> Middle // Should never get here!

    /// Get all corner tiles for the jigsaw
    let cornerTiles (jigsaw : ImageJigsaw) =
        let tiles = jigsaw.ImageTiles

        //  Note: the tiles are not neccesarily correctly oriented!
        Array.allPairs tiles tiles
        |> Array.filter (fun (t1,t2) -> t1 <> t2)
        |> Array.map (fun (t1,t2) -> (t1, snd (t1 |> ImageTile.matchTile t2)))
        |> Array.filter (fun m -> snd m <> None)
        |> Array.groupBy (fun m -> fst m)
        |> Array.filter (fun m -> snd m |> Array.length = 2)        
        |> Array.map (fst)

    /// Get top left starting corner.
    let topLeftCorner (jigsaw : ImageJigsaw) =

        // Pick an arbitrary corner and gather all other tiles.
        let corner = jigsaw |> cornerTiles |> Array.head
        let otherTiles = jigsaw.ImageTiles |> Array.filter (fun t -> t.TileId <> corner.TileId)

        // ...and rotate into position.
        let orientations = corner |> ImageTile.orientations

        let bottoms =
            Array.allPairs otherTiles orientations        
            |> Array.map (fun (t,o) -> (o,ImageTile.matchTile t o))
            |> Array.filter (fun (_,(d,_)) -> d = Some Bottom)
            |> Array.map (fun (o,(_,_)) -> o)

        Array.allPairs otherTiles bottoms        
        |> Array.map (fun (t,o) -> (o,ImageTile.matchTile t o))
        |> Array.filter (fun (_,(d,_)) -> d = Some Right)
        |> Array.map (fun (o,(_,_)) -> o)
        |> Array.head

    /// Given a tile and an edge, return the corerctly orientated tile that fits on that edge.
    let findTile (tile : ImageTile) (edge : EdgeType) (jigsaw : ImageJigsaw) =

        let otherTiles = jigsaw.ImageTiles |> Array.filter (fun t -> t.TileId <> tile.TileId)
        let found =
            Array.allPairs otherTiles [|tile|]        
            |> Array.map (fun (o,t) -> ImageTile.matchTile o t)
            |> Array.filter (fun (d,_) -> d = Some edge)
            |> Array.map (fun (_,o) -> o)
            |> Array.head

        match found with
        | Some found -> found
        | None -> ImageTile.empty

    /// Place an image tile in the specified row and column.
    let placeTile (row : int) (col : int) (jigsaw : ImageJigsaw) : ImageJigsaw =
        let positionType = positionType row col jigsaw

        let prevTile =
            match positionType with            
            | LeftEdge | BottomLeftCorner -> jigsaw.BoardGrid.[row-1].[0]            
            | TopEdge | Middle | BottomEdge | TopRightCorner | RightEdge | BottomRightCorner -> jigsaw.BoardGrid.[row].[col-1]            
            | _ -> ImageTile.empty

        let tile =
            match positionType with
            | TopLeftCorner -> (jigsaw |> topLeftCorner)
            | LeftEdge | BottomLeftCorner -> (jigsaw |> (findTile prevTile Bottom))
            | TopEdge | Middle | BottomEdge | TopRightCorner | RightEdge | BottomRightCorner -> (jigsaw |> (findTile prevTile Right))            

        jigsaw.BoardGrid.[row].[col] <- tile
        jigsaw
    
    /// Get the current jigsaw board.
    let image (jigsaw : ImageJigsaw) : char array array =                

        let height = jigsaw.BoardHeight
        let width = jigsaw.BoardWidth

        let croppedBoardGrid = Array.init height (fun _ -> Array.init width (fun _ -> ImageTile.empty))

        for row' in [0..height-1] do
            for col' in [0..width-1] do
                let tile' = jigsaw.BoardGrid.[row'].[col']                
                croppedBoardGrid.[row'].[col'] <- crop tile'
        
        let sideLength = croppedBoardGrid.[0].[0].ImageGrid.[0].Length
        let image' = Array.init (sideLength*height) (fun _ -> Array.init (sideLength*width) (fun _ -> ' '))

        for row in [0..height-1] do
            for col in [0..width-1] do
                let tile = croppedBoardGrid.[row].[col]
                let rowOffset = row*sideLength
                let colOffset = col*sideLength
                for row' in [0..(sideLength-1)] do
                    for col' in [0..(sideLength-1)] do
                        image'.[rowOffset+row'].[colOffset+col'] <- tile.ImageGrid.[row'].[col']
        image'             

    /// Display the image off all pieced together tiles.
    let displayImage (jigsaw : ImageJigsaw) =
        let image = jigsaw |> image

        for x in [0..image.Length-1] do
            for y in [0..image.Length-1] do
                printf "%c" image.[x].[y]
            printfn ""