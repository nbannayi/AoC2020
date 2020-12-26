open AoC2020.Common.Utilities

// Advent of Code 2020 day 21.

[<EntryPoint>]
let main argv =

    let foods =
        "InputFiles/Day21Input.txt" 
        |> Seq.ofFileLines
        |> Array.ofSeq
        |> Array.map (fun f ->
            let tokens = f.Split(" (contains ")
            let ingredients = tokens.[0].Split(' ') |> Set.ofArray
            let allergens = tokens.[1].Replace(")","").Replace(" ","").Split(',') |> Set.ofArray
            (allergens,ingredients))
        |> Array.sortBy (fun (a,_) -> a.Count)

    let allergens = foods |> Array.map (fun (a,_) -> a) |> Seq.ofArray |> Set.unionMany
    let ingredients = foods |> Array.map (fun (_,i) -> i) |> Seq.ofArray |> Set.unionMany

    let noAllergies =
        let ingredientsSet =
            let getIntersection foods allergen =
                foods
                |> Array.filter (fun (a,_) -> (a |> Set.contains allergen))
                |> Array.map (fun (_,i) -> i)
                |> Seq.ofArray
                |> Set.intersectMany
            allergens
            |> Set.map (fun a -> getIntersection foods a)
            |> Set.toSeq
            |> Set.unionMany
        Set.difference ingredients ingredientsSet

    let noAllergiesCount =
        foods
        |> Array.map (fun (_,i) -> i)
        |> Array.map (fun i -> Set.intersect i noAllergies)
        |> Array.map (Set.count)
        |> Array.sum

    printfn "Part 1: result is %d" noAllergiesCount

    let ingredients =

        let getIntersection foods allergen =
            let ingredients =
                foods
                |> Array.filter (fun (a,_) -> (a |> Set.contains allergen))
                |> Array.map (fun (_,i) -> i)
                |> Seq.ofArray
                |> Set.intersectMany
            (allergen, ingredients)

        let foods' = foods |> Array.map (fun (a,i) -> (a, Set.difference i noAllergies))

        allergens
        |> Set.map (fun a -> getIntersection foods' a)
        |> Set.toArray
        |> Array.sortBy (fun (_,i) -> i.Count)

    let rec getAllergens (ingredientsSet : ((string * Set<string>) [])) =        

        let wittleDown ingredients =
            let found =
                ingredients
                |> Array.filter (fun (i,a) -> a |> Set.count = 1)
                |> Array.map (fun (i,a) -> a)
                |> Seq.ofArray
                |> Set.unionMany
            ingredients |> Array.map (fun (a,i) -> (a,Set.difference i found))

        seq {
            let ingredientsSet' = ingredientsSet |> Array.filter (fun (_,i) -> i.Count = 1)
            if ingredientsSet' |> Array.length <> 0 then
                let unpack = ingredientsSet' |> Array.map (fun i -> (fst i,snd i |> Set.toArray |> Array.head))
                yield unpack
                let ingredientsSet = wittleDown ingredientsSet                
                yield! getAllergens ingredientsSet
        }

    let canonicalAllergens' =
        getAllergens ingredients
        |> Seq.collect (fun i -> i)
        |> Seq.sortBy (fst)
        |> Seq.map (fun (a,i) -> i + ",")
        |> Seq.stringSeqToString
    let canonicalAllergens = canonicalAllergens'.Substring(0, canonicalAllergens'.Length - 1)

    printfn "Part 2: result is %s" canonicalAllergens
    0    