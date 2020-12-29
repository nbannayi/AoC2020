namespace AoC2020.Common.Utilities

open AoC2020.Common.Utilities.HexTile
open System

// Hex Floor module for AoC 2020 day 24.

module HexFloor =

    /// Create an AoC day 24 Hexagonal floor, let me into the hotel!!
    type HexFloor = 
        {            
            /// Consolidated list of floor tiles.
            Tiles: Map<int * int * int, HexTile>
        }
        with 

        /// Instantiate an Hexagonal floor type.
        static member create tiles =            
            let mutable floorMap = Map.empty

            tiles
            |> List.iter (fun t ->
                if floorMap.ContainsKey t.Position then
                    floorMap <- floorMap.Add (t.Position, HexTile.flip t)
                else
                    floorMap <- floorMap.Add (t.Position, t)
            )
                            
            { Tiles = floorMap }                            

        /// Get number of tiles with black side facing up.
        member this.NoBlackTiles = 
            this.Tiles
            |> Map.toList
            |> List.filter (fun (_,t) -> t.Colour = Black)
            |> List.length

    /// Takes as input a hex floor and returns a hex floor with all tiles
    /// flipped according to HexTiles Game of Life rules.
    let rearrange floor =

        // Given a tile and a floor returns the number of neighburing black tiles in surrounding hexagon.
        let getNoNeighbouringBlackTiles tile floor =
            let X,Y,Z = tile.Position
            let positions = [(X+1,Y-1,Z);(X,Y-1,Z+1);(X-1,Y,Z+1);(X-1,Y+1,Z);(X,Y+1,Z-1);(X+1,Y,Z-1)]

            positions
            |> List.choose (fun p -> if floor.Tiles.ContainsKey p then Some floor.Tiles.[p] else None)
            |> List.filter (fun t -> t.Colour = Black)
            |> List.length

        // Given a tile and floor flips the tile based on HexTile Game of Life rules.
        let updateTile tile floor =
            let noNeighbouringBlackTiles = getNoNeighbouringBlackTiles tile floor

            match tile.Colour, noNeighbouringBlackTiles with
            | c, n when c = Black && (n = 0 || n > 2) -> HexTile.flip tile
            | c, n when c = White && n = 2 -> HexTile.flip tile
            | _, _ -> tile

        /// Return a floor with all hexagons in passed state including a one layer
        /// enclosing hexagon.
        let getEnclosingHexagon floor =
            let maxBound = 
                (floor.Tiles
                 |> Map.toList
                 |> List.map (fun (_,t) ->
                    let X,Y,Z = t.Position
                    [X;Y;Z])
                 |> List.collect (fun p -> p)         
                 |> List.map (Math.Abs)
                 |> List.max) + 1

            let tiles' =
                [for x in [-maxBound..maxBound] do
                    for y in [Math.Max(-maxBound,-x-maxBound) .. Math.Min(maxBound, -x+maxBound)] ->
                        let z = -x-y
                        let position = (x,y,z)
                        if floor.Tiles.ContainsKey position then
                            (position, floor.Tiles.[position])
                        else
                            (position, HexTile.create (Some position))]
                |> Map.ofList
                                
            { Tiles = tiles' }

        // Update all tiles.
        let tiles' =
            (floor |> getEnclosingHexagon).Tiles
            |> Map.toList
            |> List.map (fun (p,t) -> (p, updateTile t floor))
            |> Map.ofList

        { Tiles = tiles' }
