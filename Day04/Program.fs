open AoC2020.Common.Utilities
open System.Text.RegularExpressions

// Advent of Code 2020 day 4.
    
[<EntryPoint>]
let main argv =

    let validatePassportComplete (passport : Map<string, string>) : bool = 
        [|"byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"|]
        |> Array.map (fun k -> passport.ContainsKey k)
        |> Array.reduce (&&)

    let checkFields (passport : Map<string, string>) : bool =
        Regex.IsMatch(passport.["byr"], "^19[2-9][0-9]|200[0-2]$") &&
        Regex.IsMatch(passport.["iyr"], "^201[0-9]|2020$") &&
        Regex.IsMatch(passport.["eyr"], "^202[0-9]|2030$") &&
        Regex.IsMatch(passport.["hgt"], "^(59|6[0-9]|7[0-6])in|(1[5-8][0-9]|19[0-3])cm$") &&
        Regex.IsMatch(passport.["hcl"], "^#([0-9]|[a-f]){6}$") &&
        Regex.IsMatch(passport.["ecl"], "^amb|blu|brn|gry|grn|hzl|oth$") &&
        Regex.IsMatch(passport.["pid"], "^[0-9]{9}$")

    let validatePassportFields (passport : Map<string, string>) : bool = 
        match (validatePassportComplete passport) with
        | false -> false
        | true -> (checkFields passport)
        
    let parsePassportBlock (passportBlock : string) =  
        passportBlock.Trim().Replace(" ", ",").Replace("\n",",").Split(',')
        |> Array.map (fun p -> (p.Split(':').[0], p.Split(':').[1]))                               
        |> Map.ofArray

    let passports = 
        "InputFiles/Day04Input.txt"
        |> Seq.ofFileChunks "\n\n"
        |> Seq.map (parsePassportBlock)
        
    let validPassportCount validation = 
        passports
        |> Seq.map (validation)
        |> Seq.filter (fun p -> p = true)
        |> Seq.length

    printfn "Part 1: result is %d" (validPassportCount validatePassportComplete)    
    printfn "Part 2: result is %d" (validPassportCount validatePassportFields)
    0