open AoC2020.Common.Utilities.Ferry

// Advent of Code 2020 day 11.      

[<EntryPoint>]
let main argv =

    let rec getStabilisedFerry (ferry: Ferry) : Ferry =
        // ferry.display // Uncomment for visualisation.
        let ferry' = ferry.updateSeats
        if ferry' <> ferry then getStabilisedFerry ferry' else ferry    

    let ferry = Ferry.create "InputFiles/Day11Input.txt" 4 Adjacent
    let stabilisedFerry = getStabilisedFerry ferry
    printf "Part 1: result is %d\n" stabilisedFerry.countOccupiedSeats

    let ferry = Ferry.create "InputFiles/Day11Input.txt" 5 Free
    let stabilisedFerry = getStabilisedFerry ferry
    printf "Part 2: result is %d\n" stabilisedFerry.countOccupiedSeats
    0