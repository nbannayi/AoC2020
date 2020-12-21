open AoC2020.Common.Utilities.PocketDimension

// Advent of Code 2020 day 17.

[<EntryPoint>]
let main argv =
    
    let rec getCycle (pd : PocketDimension) (n : int) : PocketDimension =        
        if pd.Cycle < n then getCycle pd.generateCycle n else pd         

    let dimension3D = PocketDimension("InputFiles/Day17Input.txt", '#', '.', Cube)  
    let result = (getCycle dimension3D 6).countCubes Active        
    printfn "Part 1: result is %d" result    

    // Using real rather than example input gives 2056 but takes too long (c30 mins), optimisation required!!
    // This example input gives 848 as required in c5 mins.
    let dimension4D = PocketDimension("InputFiles/Day17ExampleInput.txt", '#', '.', Hypercube)  
    let result = (getCycle dimension4D 6).countCubes Active        
    printfn "Part 2: result is %d" result    
    0