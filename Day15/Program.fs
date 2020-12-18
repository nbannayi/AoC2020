
// Advent of Code 2020 day 15.

[<EntryPoint>]
let main argv =

    // Initial algorithm.
    let update1 (currentList : int list) : int list =

        let latestNumber = currentList |> List.last

        let lastFoundIndex =
            currentList
            |> List.tail
            |> List.tryFindIndexBack (fun cl -> cl = latestNumber)

        let nextNumber =
            match lastFoundIndex with
            | None -> 0
            | Some(lastFoundIndex) ->
                let currentSublist = currentList.[0..lastFoundIndex]
                let lastFoundIndex' =
                    currentSublist
                    |> List.tryFindIndexBack (fun sl -> sl = latestNumber)
                match lastFoundIndex' with
                | None -> 0
                | Some(lastFoundIndex') -> lastFoundIndex - lastFoundIndex' + 1
                    
        currentList @ [nextNumber]

    // More efficient algorithm but still slow - need optimising, takes around 3 mins for 30 million records.
    let update2 (elementMap : Map<int,int * int>) (element : int) (turn : int) =
        
        let (turn1, turn2) = elementMap.[element]
        let element' = if turn1 = 0 then 0 else turn2 - turn1

        // Need to refactor to take out mutability.
        let mutable elementMap' = elementMap
        if elementMap'.ContainsKey element' then
            let (_,turn2') = elementMap'.[element']
            elementMap' <- elementMap'.Add(element', (turn2', turn)) 
        else
            elementMap' <- elementMap'.Add(element', (0, turn))

        (elementMap', element', turn+1)

    // Target element of sequence to find.
    let target = 2020

    // Imperative - needs refactoring.    
    let mutable input = [0;8;15;2;12;1;4]    
    let mutable target' = target - input.Length
    let mutable i = 0
    while i < target' do        
        input <- update1 input    
        i <- i + 1    

    printfn "Part 1: result is %d" (input |> List.last)

    let target = 30000000
    let elementMap = [ (0,(0,1)) ; (8,(0,2)) ; (15,(0,3)) ; (2,(0,4)) ; (12,(0,5)) ; (1,(0,6)) ; (4,(0,7)) ] |> Map.ofList
    let element = 4
    let turn = elementMap.Count + 1
    let target' = target - elementMap.Count

    // Imperative - needs refactoring.
    let mutable input = (elementMap, element, turn)
    let mutable i = 0
    while i < target' do
        let em, elem, turn = input
        input <- update2 em elem turn
        i <- i + 1

    let _,element,_ = input

    printfn "Part 2: result is %d" element
    0