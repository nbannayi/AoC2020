open AoC2020.Common.Utilities

// Advent of Code 2020 day 5.

[<EntryPoint>]
let main argv =

    let seatCodes =
        "InputFiles/Day5Input.txt" 
        |> Seq.ofFileLines
        
    let getSeatRow (seatCode : string) : int =
        seatCode.[..6]
        |> Seq.fold (fun (l,u) elem -> if elem = 'F' then l, l+(u-l)/2 else l+(u-l)/2, u) (0,128)
        |> fst        

    let getSeatColumn (seatCode : string) : int =
        seatCode.[seatCode.Length-3..]
        |> Seq.fold (fun (l,u) elem -> if elem = 'L' then l, l+(u-l)/2 else l+(u-l)/2, u) (0,8)
        |> fst

    let getSeatUniqueId (seatCode : string) : int =
        (getSeatRow seatCode) * 8 + (getSeatColumn seatCode)

    // Use sum of n natural numbers and compare with actual sum to find the missing seat.
    let getMissingSeat (seatCodes : string seq) =

        let seatUniqueIds = 
            seatCodes
            |> Seq.map (fun s -> getSeatUniqueId s)
            |> Seq.sort

        let firstSeat = (seatUniqueIds |> Seq.head) - 1
        let lastSeat = seatUniqueIds |> Seq.last
        let actualSeatSum = seatUniqueIds |> Seq.sum
        let expectedSeatSum = (lastSeat*(lastSeat+1) - firstSeat*(firstSeat+1))/2

        expectedSeatSum - actualSeatSum
                                  
    let maxUniqueSeatId =
        seatCodes
        |> Seq.map (fun s -> getSeatUniqueId s)
        |> Seq.max

    printf "Part 1: result is %d\n" maxUniqueSeatId 
    printf "Part 2: result is %A\n" (getMissingSeat seatCodes)
    0