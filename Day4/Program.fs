open AoC2020.Common.Utilities
open System.Text.RegularExpressions

// Advent of Code 2020 day 4.
    
[<EntryPoint>]
let main argv =

    let validatePassportComplete (passport : Map<string, string>) : bool = 
        [|"byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"|]
        |> Array.map (fun k -> (passport |> Map.containsKey k))
        |> Array.reduce (&&)
    
    let checkFields (passport : Map<string, string>) : bool = 
        let byr = int passport.["byr"]
        let iyr = int passport.["iyr"]
        let eyr = int passport.["eyr"]
        let hgt = passport.["hgt"]

        let checkHeight (height : string) : bool =
            if height.Length >= 4 && height.Length <= 5 then
                let unit = height.[(height.Length-2)..]
                let value = int height.[..(height.Length-3)]
                match unit with
                | "in" -> value >= 59 && value <= 76
                | "cm" -> value >= 150 && value <= 193
                | _ -> false
            else
                false   
        
        (byr >= 1920 && byr <= 2002) &&
        (iyr >= 2010 && iyr <= 2020) &&
        (eyr >= 2020 && eyr <= 2030) &&
        (checkHeight hgt) &&
        Regex.IsMatch(passport.["hcl"], "^#([0-9]|[a-f]){6}$") &&
        [|"amb";"blu";"brn";"gry";"grn";"hzl";"oth"|] |> Array.contains passport.["ecl"] &&
        Regex.IsMatch(passport.["pid"], "^[0-9]{9}$")
        
    let validatePassportFields (passport : Map<string, string>) : bool = 
        let completeness = validatePassportComplete passport

        match completeness with
        | false -> false
        | true -> (checkFields passport)
        
    let parsePassportBlock (passportBlock : string) =  
        passportBlock.Trim().Replace(" ", ",").Replace("\n",",").Split(',')
        |> Array.map (fun p -> let tokens = p.Split(':')
                               (tokens.[0], tokens.[1]))
        |> Map.ofArray

    let passports = 
        "InputFiles/Day4Input.txt"
        |> Seq.ofFileChunks "\n\n"
        |> Seq.map (parsePassportBlock)
        
    let validPassportCount validation = 
        passports
        |> Seq.map (validation)
        |> Seq.filter (fun p -> p = true)
        |> Seq.length

    let checkPassport = validatePassportComplete
    printf "Part 1: result is %d\n" (validPassportCount checkPassport)

    let checkPassport = validatePassportFields
    printf "Part 2: result is %d\n" (validPassportCount checkPassport)
    0