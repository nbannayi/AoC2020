open AoC2020.Common.Utilities

// Advent of Code 2020 day 23.

[<EntryPoint>]
let main argv =

    let getFinalCups cups =
        let noCups = cups |> Array.length
        let positionOf1 = cups |> Array.findIndex (fun c -> c = 1)
        [|for i in [1..(noCups-1)] -> (positionOf1+i) % noCups|]
        |> Array.map (fun i -> cups.[i])
        |> Array.map (string)
        |> Array.reduce (+)        

    // Initial (naive) implementation - much too slow for part 2!
    let playCups1 n cups =        

        let move cups currentCup : int array * int =
            let getNext3Cups cups currentCup =
                let noCups = cups |> Array.length
                let startPos = cups |> Array.findIndex (fun c -> c = currentCup)
                [|(startPos+1) % noCups; (startPos+2) % noCups; (startPos+3) % noCups|]
                |> Array.map (fun c -> cups.[c])

            let getDestinationCup cups currentCup =
                let cupsMinusNext3Cups = cups |> Array.except (getNext3Cups cups currentCup)
                let cupsSequence =
                    Array.append (cupsMinusNext3Cups |> Array.filter (fun c -> c < currentCup) |> Array.sortDescending)
                                 (cupsMinusNext3Cups |> Array.filter (fun c -> c > currentCup) |> Array.sortDescending)
                cupsSequence.[0]

            let getNextCup cups currentCup =
                let noCups = cups |> Array.length
                let startPos = cups |> Array.findIndex (fun c -> c = currentCup)
                cups.[(startPos+1) % noCups]

            let next3Cups = getNext3Cups cups currentCup
            let cupsMinusNext3Cups = cups |> Array.except next3Cups
            let destinationCup = getDestinationCup cups currentCup
            let destinationCupPos = cupsMinusNext3Cups |> Array.findIndex (fun c -> c = destinationCup)        
            let lhsCups = cupsMinusNext3Cups.[0..destinationCupPos]
            let rhsCups = cupsMinusNext3Cups.[(destinationCupPos+1)..]
            let finalCups = [|lhsCups;next3Cups;rhsCups|] |> Array.collect (fun c -> c)
            let nextCup = getNextCup finalCups currentCup
            (finalCups, nextCup)

        [|1..n|]
        |> Array.fold (fun (cups,nextCup) n ->
            move cups nextCup) (cups, cups.[0])      

    // More optimised solution...
    let getNextTwoCupsProduct cups =
        let noCups = cups |> List.length
        let positionOf1 = cups |> List.findIndex (fun c -> c = 1)

        let star1 = cups.[(positionOf1+1) % noCups] |> int64
        let star2 = cups.[(positionOf1+2) % noCups] |> int64
        star1 * star2

    let getDestinationCup currentCup cups picked =
        let mutable found = true
        let mutable destinationCup = currentCup

        // TODO: refactor at some point to make immutable.
        while found do
            destinationCup <- destinationCup - 1
            destinationCup <- if destinationCup = 0 then cups |> List.max else destinationCup
            found <- List.contains destinationCup picked

        destinationCup

    // TODO: performance is fairly good but refactor at some point to try to make
    // solution immutable.
    let playCups2 cups moves =

        // Create circular linked list.
        let mutable cupPointers = Array.init (moves + 1) (fun _ -> -1)
        cupPointers.[cups |> List.last] <- cups |> List.head
        for w in cups |> List.windowed 2 do cupPointers.[w.[0]] <- w.[1]

        // Get current cup at head of list.
        let mutable currentCup = cups |> List.head

        for _ in [0..moves-1] do

            // Get next three cups.
            let mutable next3Cups = [cupPointers.[currentCup]]
            for _ in [0..1] do
                let lastCup = next3Cups |> List.last
                next3Cups <- next3Cups @ [cupPointers.[lastCup]]

            // Point to the cup after the last of the next three picked cups.
            cupPointers.[currentCup] <- cupPointers.[List.last next3Cups]

            // Get destination cup
            let destinationCup = getDestinationCup currentCup cups next3Cups

            // Place three cups after destination cup.
            let nextToDest = cupPointers.[destinationCup]
            cupPointers.[destinationCup] <- next3Cups |> List.head
            cupPointers.[next3Cups |> List.last] <- nextToDest
            currentCup <- cupPointers.[currentCup]

        // Return the entire sequence.
        seq {
            let mutable cup = 1
            yield cup
            while cupPointers.[cup] <> 1 do
                yield cupPointers.[cup]
                cup <- cupPointers.[cup]
        }
        
    // Initial solution.
    let cups1 = "327465189" |> String.toIntSeq |> Array.ofSeq    
    let f1() = (playCups1 100 cups1)
    let result1 = Timer.duration "Part 1" f1
    printfn "Part 1: result is %A" (result1 |> fst |> getFinalCups)

    // Use circular linked list based implementation.
    let cups2 = ("327465189" |> String.toIntSeq |> List.ofSeq) @ [10..1000000]
    let f2() = (playCups2 cups2 10000000)
    let result2 = Timer.duration "Part 2" f2    
    printfn "Part 2: result is %A" (result2 |> List.ofSeq |> getNextTwoCupsProduct) 
    0