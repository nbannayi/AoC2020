open AoC2020.Common.Utilities

// Advent of Code 2020 day 2.

[<EntryPoint>]
let main argv =
   
    let inputPasswordLines = 
        "InputFiles/Day2Input.txt" 
        |> Seq.ofFile 
        |> Seq.map (fun s -> let tokens = s.Split(':') 
                             (tokens.[0].Trim(), tokens.[1].Trim()))
                            
    let getPolicyDetails (policy : string) : int * int * char =  
        let policyTokens = policy.Split(" ")
        let policyCharRange = policyTokens.[0].Split("-") |> Array.map (int) 
        (policyCharRange.[0], policyCharRange.[1], policyTokens.[1] |> char)        

    let validatePassword1 (policy : string) (password : string) : bool =
        let policyLower, policyUpper, policyLetter = getPolicyDetails policy
        let passwordLetterCount =
            password
            |> String.collect(fun p -> (if p = policyLetter then policyLetter |> string else ""))
            |> String.length
        passwordLetterCount >= policyLower && passwordLetterCount <= policyUpper

    let validatePassword2 (policy : string) (password : string) : bool =
        let policyLower, policyUpper, policyLetter = getPolicyDetails policy
        (password.[policyLower-1] =  policyLetter && password.[policyUpper-1] <> policyLetter) ||
        (password.[policyLower-1] <> policyLetter && password.[policyUpper-1] =  policyLetter)

    let validPasswordCount (input : seq<string * string>) rule = 
        input
        |> Seq.map (fun (policy, password) -> rule policy password)            
        |> Seq.filter (fun i -> i = true)
        |> Seq.length

    printf "Part 1: result is %d\n" (inputPasswordLines |> validPasswordCount <| validatePassword1)
    printf "Part 2: result is %d\n" (inputPasswordLines |> validPasswordCount <| validatePassword2)
    0
