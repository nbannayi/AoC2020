open AoC2020.Common.Utilities

// Advent of Code 2020 day 16.

[<EntryPoint>]
let main argv =

    let ticketData =
        "InputFiles/Day16Input.txt" 
        |> Seq.ofFileChunks "\n\n"
        |> Array.ofSeq

    let parseTicketRule (ticketRule : string) : string * int array =
        let parseRanges (ranges : string) : int array =
            let parseRange (range : string) : int array =
                let tokens = range.Split("-")
                let lb, up = (int tokens.[0], int tokens.[1])
                [|lb..up|]
            ranges.Split(" or ")
            |> Array.map (parseRange)
            |> Array.concat
        let tokens = ticketRule.Split(": ")
        let key = tokens.[0]
        let values = parseRanges tokens.[1]
        key, values

    // Ticket rules in a map preserving structure.
    let ticketRules =
        ticketData.[0].Split('\n')
        |> Array.map (parseTicketRule)
        |> Map

    // Ticket rules flattened to a set for easy searching.
    let ticketRulesSet =
        ticketRules |> Map.toArray |> Array.map (fun a -> snd a) |> Array.concat |> Set

    let nearbyTickets =
        ticketData.[2].Split('\n') |> Array.tail |> Array.map (fun nt -> nt.Split(',') |> Array.map (int))

    let getInvalidTicketCodes ticketRuleSet ticket =
        let isTicketCodeInValid ticketRulesSet ticketCode =
            not (ticketRulesSet |> Set.contains ticketCode)
        ticket
        |> Array.filter (isTicketCodeInValid ticketRuleSet)

    let invalidTicketCodes =
        nearbyTickets
        |> Array.map (getInvalidTicketCodes ticketRulesSet)
        |> Array.concat
    
    printfn "Part 1: result is %d" (invalidTicketCodes |> Array.sum)

    // Discard all invalid tickets.
    let validTickets =
        nearbyTickets
        |> Array.filter (fun t -> (getInvalidTicketCodes ticketRulesSet t |> Array.length) = 0)

    // Work out ticket categories in order.
    let categories =
        let getTicketColumnPossibleCategories tickets ticketRules column =
            let getTicketCodeCategory (ticketRules : Map<string,int array>) (ticketCode : int) =
                [|for (key,value) in (ticketRules |> Map.toArray) do            
                    if value |> Array.contains ticketCode then key else ""|]
                |> Array.filter (fun t -> t <> "")
            tickets
            |> Array.map (fun (t : int array) -> t.[column])
            |> Array.map (fun tc -> getTicketCodeCategory ticketRules tc)
            |> Array.map (Set)
            |> Array.reduce (Set.intersect)

        let categories' =
            validTickets.[0]
            |> Array.mapi (fun i _ -> getTicketColumnPossibleCategories validTickets ticketRules i)
            |> Array.mapi (fun i s -> (s.Count,i,s))
            |> Array.sortBy (fun (c,_,_) -> c)
            |> Array.map (fun (_,i,s) -> (i,s))

        let cat1 = categories'.[1..]
        let cat2 = categories'.[0..categories'.Length-2]    

        Array.zip cat1 cat2
        |> Array.map (fun (c1,c2) ->
            let i,s1 = c1
            let _,s2 = c2
            (i, Set.difference s1 s2))
        |> Array.append [|cat2.[0]|]
        |> Array.sortBy (fst)
        |> Array.map (fun s -> (snd s) |> Set.toArray |> Array.head)

    // Finally get the ones we're interested in.
    let departureCategories =
        categories
        |> Array.indexed
        |> Array.filter (fun (i,c) -> c.Contains("departure"))

    let myTicket =
        ticketData.[1].Split('\n').[1].Split(',') |> Array.map (int64)

    let (result : int64) =
        departureCategories
        |> Array.map (fun (i,_) -> myTicket.[i])
        |> Array.reduce (*)

    printfn "Part 2: result is %d" result
    0