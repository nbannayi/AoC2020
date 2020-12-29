namespace AoC2020.Common.Utilities

// Hex Tile module for AoC 2020 day 24.

module HexTile =

    /// Tile colour (different colour one each side.)
    type TileColour =
        | White
        | Black

    /// Direction to move tile.     
    type Direction =
        | E | SE | SW | W | NW | NE | Unknown

        static member parse direction =
            match direction with
            | "e"  -> E
            | "se" -> SE
            | "sw" -> SW
            | "w"  -> W
            | "nw" -> NW
            | "ne" -> NE
            | _    -> Unknown

    /// Create an AoC day 24 Hexagon Tile.
    type HexTile = 
        {
            /// Position of tile using 3 dimensional coordinate system.
            /// Details contained here https://www.redblobgames.com/grids/hexagons/
            Position: int * int * int // (X,Y,Z) 
            /// Tile colour (both sides are coloured differently.)
            Colour: TileColour
        }

        /// Instatiate a tile in initial state at (0,0,0).
        static member create (position : (int * int * int) option) =
            { 
                Position = Option.defaultValue (0,0,0) position
                Colour = White
            }

    /// Flip the tile over and return flipped tile.
    let flip (tile : HexTile) : HexTile =
        let newColour =
            if tile.Colour = White then Black else White

        { tile with Colour = newColour }

    /// Move tile one place in direction specified and return tile.
    let move (direction : Direction) (tile : HexTile) : HexTile =
        let X,Y,Z = tile.Position

        let position' = 
            match direction with
            | E  -> (X+1,Y-1,Z)
            | SE -> (X,Y-1,Z+1)
            | SW -> (X-1,Y,Z+1)
            | W  -> (X-1,Y+1,Z)
            | NW -> (X,Y+1,Z-1)
            | NE -> (X+1,Y,Z-1)
            | Unknown -> failwith "Something went wrong!"

        // Set to black by default at destination, but may be overriden depending on tile already there.
        { Position = position'; Colour = Black }

    