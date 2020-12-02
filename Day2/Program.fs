open AoC2020.Common.Utilities

// Advent of Code 2020 day 2.

[<EntryPoint>]
let main argv =
           
    // Get puzzle input.
    let inputPasswordLines = 
        "InputFiles/Day2Input.txt" 
        |> Seq.ofFile 
        |> Seq.map (fun s -> let tokens = s.Split(':') 
                             (tokens.[0].Trim(), tokens.[1].Trim()))

    // Validate password matches policy in part 1.
    let validatePassword1 (policy : string) (password : string) : bool =
        let policyTokens = policy.Split(" ")  
        let policyCharRange = policyTokens.[0].Split("-") |> Array.map (int) 
        let policyLetter = policyTokens.[1] |> char

        let passwordLetterCount =
            password
            |> String.collect(fun p -> (if p = policyLetter then policyLetter |> string else ""))
            |> String.length
            
        passwordLetterCount >= policyCharRange.[0] && passwordLetterCount <= policyCharRange.[1]

    // Validate password matches policy in part 2.
    let validatePassword2 (policy : string) (password : string) : bool =
        let policyTokens = policy.Split(" ")  
        let policyCharRange = policyTokens.[0].Split("-") |> Array.map (int) 
        let policyLetter = policyTokens.[1] |> char

        (password.[policyCharRange.[0]-1] =  policyLetter && password.[policyCharRange.[1]-1] <> policyLetter) ||
        (password.[policyCharRange.[0]-1] <> policyLetter && password.[policyCharRange.[1]-1] =  policyLetter)

    // Count number of valid passwords from passed in rule.
    let validPasswordCount (input : seq<string * string>) rule = 
        input
        |> Seq.map (fun (policy, password) -> rule policy password)            
        |> Seq.filter (fun i -> i = true)
        |> Seq.length

    printf "Part 1: result is %d\n" (inputPasswordLines |> validPasswordCount <| validatePassword1)
    printf "Part 2: result is %d\n" (inputPasswordLines |> validPasswordCount <| validatePassword2)

    0
