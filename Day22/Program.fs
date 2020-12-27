open AoC2020.Common.Utilities

// Advent of Code 2020 day 22.

[<EntryPoint>]
let main argv =

    let packs =
        "InputFiles/Day22Input.txt" 
        |> Seq.ofFileChunks ("\n\n")
        |> Array.ofSeq
        |> Array.map (fun c -> c.Split('\n') |> Array.tail |> Array.map (int))
    let p1Cards, p2Cards = packs.[0], packs.[1]

    let getWinner (p1Cards : int array, p2Cards : int array) : int * int =
        let packScore cards =
            cards
            |> Array.rev
            |> Array.mapi (fun i c -> (i+1) * c)
            |> Array.sum
        if p1Cards.Length = 0 then (2, packScore p2Cards) else (1, packScore p1Cards)

    let rec playCombat (p1Cards : int array, p2Cards : int array) : int array * int array =

        let playRound (p1Cards : int array, p2Cards : int array) : int array * int array =        
            let p1TopCard, p2TopCard = p1Cards.[0], p2Cards.[0]
            let p1Cards', p2Cards' = p1Cards |> Array.tail, p2Cards |> Array.tail

            if p1TopCard > p2TopCard then
                (Array.append p1Cards' [|p1TopCard; p2TopCard|], p2Cards')
            else
                (p1Cards', Array.append p2Cards' [|p2TopCard; p1TopCard|])

        let p1Cards, p2Cards = playRound (p1Cards, p2Cards)
        if p1Cards.Length = 0 || p2Cards.Length = 0 then (p1Cards, p2Cards) else playCombat (p1Cards, p2Cards)                        
    
    let winner1 = (p1Cards, p2Cards) |> playCombat |> getWinner
    printfn "Part 1: result is %d" (snd winner1)       

    let rec playCombatRecursive previousCards (cards: int list list) =

        let scoreCards cards =
            cards
            |> List.rev
            |> List.mapi (fun i c -> (i+1) * c)
            |> List.sum

        let previousCards' =
            previousCards
            |> Set.add (cards |> List.map scoreCards)

        if previousCards'.Count = previousCards.Count then
            1, 2
        else
            let p1Cards = cards.[0]
            let p2Cards = cards.[1]            
            
            match p1Cards, p2Cards with
            | p1Top::p1Cards', p2Top::p2Cards' when p1Cards'.Length >= p1Top && p2Cards'.Length >= p2Top ->
                let subgame = playCombatRecursive Set.empty [p1Cards' |> List.take p1Top; p2Cards' |> List.take p2Top]
                if fst subgame = 1 then
                    playCombatRecursive previousCards' [p1Cards' @ [p1Top; p2Top]; p2Cards']
                else
                    playCombatRecursive previousCards' [p1Cards'; p2Cards' @ [p2Top; p1Top]]
            | p1Top::p1Cards', p2Top::p2Cards' when p1Top > p2Top ->
                playCombatRecursive previousCards' [p1Cards' @ [p1Top; p2Top]; p2Cards']
            | p1Top::p1Cards', p2Top::p2Cards' when p1Top < p2Top ->
                playCombatRecursive previousCards' [p1Cards'; p2Cards' @ [p2Top; p1Top]]
            | p1Cards, [] ->
                1, scoreCards p1Cards
            | [], p2Cards ->
                2, scoreCards p2Cards
            | _, _ ->
                0, 0

    let winner2 =
        playCombatRecursive Set.empty [p1Cards |> List.ofArray; p2Cards |> List.ofArray]        

    printfn "Part 2: result is %A" (winner2 |> snd)  
    0