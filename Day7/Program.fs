open AoC2020.Common.Utilities

// Advent of Code 2020 day 7.

type Bag = { Colour: string; ContainedBags: Bag array } 
    with 
        member this.ContainsBag(bag : Bag) : bool = 
            this.ContainedBags |> Array.contains bag

[<EntryPoint>]
let main argv =
       
    let processBagRule (rule : string) : Bag =
        let tokens = rule.Split("contain")
        let colour = tokens.[0].[..tokens.[0].IndexOf("bags")-2]

        let processBagContentsRule (contentsRule : string) : Bag array =
            if contentsRule = "no other" then
                [||]
            else
                let noBags = int contentsRule.[..contentsRule.IndexOf(" ")-1]
                let bag = { Colour = contentsRule.[contentsRule.IndexOf(" ")+1..]; ContainedBags = [||]}
                Array.init noBags (fun _ -> bag)

        let bagContentsRules = 
            tokens.[1].Replace(" bags"," ").Replace("bag", "").Replace(".", "").Split(',')
            |> Array.map (fun s -> s.Trim())
            |> Array.collect (processBagContentsRule)

        { Colour = colour; ContainedBags = bagContentsRules }

    let bagsToSearch =
        "InputFiles/Day7Input.txt" 
        |> Seq.ofFileLines
        |> Seq.map (processBagRule)
        |> Array.ofSeq

    let getOuterBags (bagsToSearch : Bag array) (bagsToFind : Bag array) =
        let findBags (bagsToSearch : Bag array) (bagToFind : Bag) : Bag array =
            bagsToSearch
            |> Array.filter (fun b -> b.ContainsBag(bagToFind))

        bagsToFind
        |> Array.collect (fun bf -> findBags bagsToSearch bf)
        |> Array.map (fun bf -> { Colour = bf.Colour; ContainedBags = [||] })
        |> Array.distinct

    let getInnerBags (bagsToSearch : Bag array) (bagsToFind : Bag array) : Bag array =            
        bagsToFind
        |> Array.map (fun bf -> bagsToSearch |> Array.tryFind (fun bf' -> bf'.Colour = bf.Colour))
        |> Array.collect (fun bf -> match bf with
                                    | Some(bf) -> bf.ContainedBags
                                    | None -> [||])

    let rec getAllBags (bagsToSearch : Bag array) (bagsToFind : Bag seq) (mode : string) : Bag seq =
        let bagFunction = if mode = "inner" then getInnerBags bagsToSearch else getOuterBags bagsToSearch

        seq {
            let allBags = (bagFunction (bagsToFind |> Array.ofSeq)) |> seq
            if (allBags |> Seq.length) > 0 then
                yield! allBags
                yield! getAllBags bagsToSearch allBags mode
        }
        
    let bagsToFind = seq { yield {Colour = "shiny gold"; ContainedBags = [||]} }
    let outerBagCount = getAllBags bagsToSearch bagsToFind "outer" |> set |> Set.count
    let innerBagCount = getAllBags bagsToSearch bagsToFind "inner" |> Seq.length

    printf "Part 1: result is %d\n" outerBagCount
    printf "Part 2: result is %d\n" innerBagCount    
    0