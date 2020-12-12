namespace AoC2020.Common.Utilities

// Ferry class for AoC 2020 day 11.

open System

module Ferry = 

    /// Represents the mode for finding free seats.
    type SeatSearchMode =
        | Adjacent // Part 1.
        | Free     // Part 2.

    /// Create an AoC day 11 Ferry.
    type Ferry = 
        {
            Seats: string array 
            MinSeatsRequired: int
            SeatSearchMode: SeatSearchMode  
        } 
        with 

        /// Instatiate a Ferry type with layout file, minimum seats to look for and seat search mode.
        static member create layoutFile minSeatsRequired mode =
            { 
                Seats = layoutFile |> Seq.ofFileLines |> Array.ofSeq
                MinSeatsRequired = minSeatsRequired
                SeatSearchMode = mode
            }

        /// Get seating area width.
        member this.SeatingAreaWidth = 
            this.Seats.[0].Length

        /// Get seating area height.
        member this.SeatingAreaHeight =
            this.Seats.Length

        /// Get the symbol for a specific seat location.
        member this.getSeat row col : char = 
            this.Seats.[row].[col]

        /// Count all currently occupied seats.
        member this.countOccupiedSeats =
            this.Seats
            |> Array.map (fun aisle -> aisle |> Seq.filter (fun c -> c = '#') |> Seq.length)
            |> Array.sum

        /// Show ASCII display of seating area.
        member this.display =

            Console.SetCursorPosition(0, 0)

            this.Seats 
            |> Array.iter (fun aisle -> 
                aisle |> Seq.iter (fun c ->
                    if c = 'L' then                        
                        Console.ForegroundColor <- ConsoleColor.DarkRed
                    elif c = '#' then
                        Console.ForegroundColor <- ConsoleColor.Red          
                    elif c = '.' then
                        Console.ForegroundColor <- ConsoleColor.Yellow
                    printf ".") 
                printf "\n")

        /// Count adjacent occupied seats in 8 directions from passed location.
        member this.countAdjacentOccupiedSeats row col = 
            let adjacentSeatLocations = 
                [|(row-1, col-1); (row-1, col); (row-1, col+1); (row, col-1); (row, col+1); (row+1, col-1); (row+1, col); (row+1, col+1)|]
                |> Array.filter (fun (r, c) -> r >= 0 && r <= this.SeatingAreaHeight-1 && c >= 0 && c <= this.SeatingAreaWidth-1)
                
            adjacentSeatLocations
            |> Array.map (fun (r, c) -> this.getSeat r c)
            |> Array.filter (fun c -> c = '#')
            |> Array.length                          

        /// Count first free occupied seat looking in all directions.
        member this.countFirstFreeOccupiedSeats row col =
            let find path =
                path 
                |> Array.filter (fun (r,c) -> r >= 0 && r <= this.SeatingAreaHeight-1 && c >= 0 && c <= this.SeatingAreaWidth-1)
                |> Array.map (fun (r,c) -> this.getSeat r c) |> Array.tryFind (fun c -> c = '#' || c = 'L') 

            let rt = [|col+1..this.SeatingAreaWidth-1|] |> Array.map (fun r -> (row, r))
            let lt = [|col-1..-1..0|] |> Array.map (fun l -> (row, l))
            let dn = [|row+1..this.SeatingAreaHeight-1|] |> Array.map (fun d -> (d, col))
            let up = [|row-1..-1..0|] |> Array.map (fun u -> (u, col))
            let rtup = Seq.zip [|row-1..-1..0|] [|col+1..this.SeatingAreaWidth-1|] |> Array.ofSeq
            let rtdn = Seq.zip [|row+1..this.SeatingAreaHeight-1|] [|col+1..this.SeatingAreaWidth-1|] |> Array.ofSeq
            let ltup = Seq.zip [|row-1..-1..0|] [|col-1..-1..0|] |> Array.ofSeq 
            let ltdn = Seq.zip [|row+1..this.SeatingAreaHeight-1|] [|col-1..-1..0|] |> Array.ofSeq

            [|rt;lt;dn;up;rtup;rtdn;ltup;ltdn|]
            |> Array.map (fun p -> match find p with | None -> 0 | Some('#') -> 1 | _ -> 0)
            |> Array.sum

        /// Update a seat based on rules.
        member this.updateSeat row col : char =
            let seatCounter = 
                match this.SeatSearchMode with 
                | Adjacent -> this.countAdjacentOccupiedSeats row col
                | Free -> this.countFirstFreeOccupiedSeats row col

            let currentSeat = this.getSeat row col 
            match currentSeat, seatCounter with
            | 'L', 0 -> '#'
            | '#', occupied -> if occupied >= this.MinSeatsRequired then 'L' else '#'
            | _, _ -> currentSeat 

        /// Update all seats based on rules and return an updated Ferry.
        member this.updateSeats =

            let seats = 
                seq { for row in 0..this.SeatingAreaHeight-1 do
                        for col in 0..this.SeatingAreaWidth-1 -> this.updateSeat row col}
                |> Seq.chunkBySize this.SeatingAreaWidth
                |> Seq.map (fun a -> a |> Array.map (string) |> Array.reduce (+))
                |> Array.ofSeq

            { Seats = seats; MinSeatsRequired = this.MinSeatsRequired; SeatSearchMode = this.SeatSearchMode }  