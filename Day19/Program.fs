open AoC2020.Common.Utilities
open System.Text.RegularExpressions

// Advent of Code 2020 day 19.

[<EntryPoint>]
let main argv =

    let messageData =
        "InputFiles/Day19Input.txt" 
        |> Seq.ofFileChunks ("\n\n")
        |> Array.ofSeq

    let rules =
        messageData.[0].Split('\n')
        |> Array.map (fun md ->
            let tokens = md.Split(": ")
            let key = tokens.[0]
            let values = tokens.[1].Split(' ') |> Array.map (fun v -> v.Replace("\"","")) |> Seq.ofArray 
            (key,values))
        |> Map.ofArray

    let messages = messageData.[1].Split('\n')

    let getRegex rules =

        let rec getRegexSeq rules rulesSeq =

            let translateSeq rules rulesSeq =
                rulesSeq
                |> Seq.map (fun r ->
                    let values = rules |> Map.tryFind r
                    match values with
                    | Some values -> seq [")"] |> Seq.append (values |> Seq.append ["("])
                    //| Some values -> values
                    | None -> seq[r])
                |> Seq.collect (fun r -> r)                

            let regexComplete regexSeq : bool =
                let regexLength = regexSeq |> Seq.length
                let targetLength =
                    regexSeq
                     |> Seq.filter (fun s -> s = "|" || s = "a" || s = "b" || s = "(" || s = ")")
                     |> Seq.length

                // For part 2: by trial and error realised that the sequence eventually repeats
                // infinitely.  After c60,000 characters it stablises and contains at least 1 full
                // iteration so use the Regex at this point.
                //                              vvv
                (regexLength = targetLength) || regexLength > 60000
            
            let rulesSeq = translateSeq rules rulesSeq               
            if not (regexComplete rulesSeq) then getRegexSeq rules rulesSeq else rulesSeq

        "^" + ((getRegexSeq rules rules.["0"]) |> Seq.stringSeqToString) + "$"

    let regex1 = getRegex rules
    
    let result1 =
        messages
        |> Array.map (fun m -> Regex.IsMatch(m, regex1))
        |> Array.filter (fun b -> b = true)
        |> Array.length

    printfn "Part 1: result is %d" result1
 
    let rules' =
        rules
        |> Map.remove "8"
        |> Map.remove "11"
        |> Map.add "8" (seq ["42";"|";"42";"8"])
        |> Map.add "11" (seq ["42";"31";"|";"42";"11";"31"])   

    let regex2 = getRegex rules'

    let result2 =
        messages
        |> Array.map (fun m -> Regex.IsMatch(m, regex2))
        |> Array.filter (fun b -> b = true)
        |> Array.length

    printfn "Part 2: result is %d" result2    
    0