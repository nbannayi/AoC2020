namespace AoC2020.Common.Utilities

// Pocket Dimension module for AoC 2020 day 17.

module PocketDimension =

    /// Represents the state of a Conway cube.
    type CubeState =
        | Active
        | Inactive

    /// 3 dimensional or 4 dimensional space.
    type CubeMode =
        | Cube
        | Hypercube

    /// Represents a Conway cube.
    type Cube =
        {
            Position : int * int * int * int // (W,Z,X,Y)
            State: CubeState
        }
        with
            member this.toString : string =
                let w,z,x,y = this.Position
                sprintf "(%d %d %d %d: %A)" w z x y this.State

    /// Create an AoC day 17 Pocket Dimension.
    type PocketDimension(initialStateFile : string, activeSymbol : char, inactiveSymbol : char, mode : CubeMode) =                   

        let mutable cycle = 0
        
        let mutable grid =
            initialStateFile                    
            |> Seq.ofFileLines
            |> Seq.map (String.toCharSeq)
            |> Seq.mapi (fun x xs ->
                xs |> Seq.mapi (fun y c ->                            
                    { Position = (0,0,x,y); State = if c = activeSymbol then Active else Inactive }))
                |> Seq.collect (fun c -> c)
            |> List.ofSeq

        let range =
            let wRange = if mode = Hypercube then [|-1..1|] else [|0|] 
            [for w' in wRange do
                for z' in [|-1..1|] do
                    for x' in [|-1..1|] do 
                        for y' in [|-1..1|] -> (w',z',x',y')]

        /// Return number of cubes in the given state.
        member this.countCubes (state : CubeState) : int =
            grid
            |> List.filter (fun c -> c.State = state)
            |> List.length

        /// Get cube in the grid by position.
        member this.getCube position : Cube option =
            let w,z,x,y = position
            grid
            |> List.tryFind (fun c -> c.Position = (w,z,x,y))  
        
        /// Get all neighbouring locations of the passed cube.
        member this.getNeighbouringCubeLocations position =
            let w,z,x,y = position

            range
            |> List.map (fun (w',z',x',y') -> (w+w',z+z',x+x',y+y'))
            |> List.filter (fun (w',z',x',y') -> (w',z',x',y') <> (w,z,x,y))            

        /// Update an individual cube status and return a cube.
        member this.processCubeLocation position : Cube =

            let noActiveNeighbouringCubes =
                position
                |> this.getNeighbouringCubeLocations
                |> List.map (fun p -> match (this.getCube p) with | Some(c) -> (if c.State = Active then 1 else 0) | None -> 0)
                |> List.sum
            
            let state =
                match this.getCube position with
                | Some c -> c.State
                | None -> Inactive

            let newState =
                match state with
                | Active -> if noActiveNeighbouringCubes = 2 || noActiveNeighbouringCubes = 3 then Active else Inactive
                | Inactive -> if noActiveNeighbouringCubes = 3 then Active else Inactive
           
            { Position = position; State = newState }

        /// Get all active cubes in grid.
        member this.ActiveCubes =
            grid
            |> List.filter (fun c -> c.State = Active)

        /// Get box enclosing all active cubes for the next cycle.
        member this.getBox =

            let getRange axis =
                let fn =
                    match axis with
                    | 'w' -> (function (w,_,_,_) -> w)
                    | 'z' -> (function (_,z,_,_) -> z)
                    | 'x' -> (function (_,_,x,_) -> x)
                    | 'y' -> (function (_,_,_,y) -> y)
                    | _ -> (function (_,_,_,_) -> 0)

                let ps = grid |> List.map (fun c -> c.Position) |> List.map (fn) |> List.sort
                let p0 = if mode = Cube && axis = 'w' then 1 else ps |> List.head
                let p1 = if mode = Cube && axis = 'w' then -1 else ps |> List.last
                [p0-1..p1+1]

            [for w' in getRange 'w' do
                for z' in getRange 'z' do
                    for x' in getRange 'x' do 
                        for y' in getRange 'y' -> (w',z',x',y')]

        /// Display all cubes in all 4 dimensions.
        member this.display =

            let firstCubePosition = this.getBox.Head
            let lastCubePosition = (this.getBox |> List.last)

            let w,z,x,y = firstCubePosition
            let w',z',x',y' = lastCubePosition

            for wi in [w..w'] do
                printfn "w=%d" wi
                for zi in [z..z'] do
                    printfn "z=%d" zi
                    for xi in [x..x'] do
                        for yi in [y..y'] do                         
                            let cube = this.getCube (wi,zi,xi,yi)
                            match cube with
                            | Some(c) -> if c.State = Active then printf "%c" activeSymbol else printf "%c" inactiveSymbol
                            | None -> printf "%c" inactiveSymbol
                        printfn ("<")                

            printfn "\nCycle: %d, active %d" cycle (this.countCubes Active)

        /// Regenerate all cubes.
        member this.generateCycle =

            let box = this.getBox
            let boxCount = box.Length

            grid <-
                this.getBox
                |> List.mapi (fun i p ->
                        // printfn "%d/%d" i boxCount // Uncomment to display progress in each cycle.
                        this.processCubeLocation p)
                |> List.filter (fun c -> c.State = Active)

            cycle <- cycle + 1            
            //this.display // Uncomment to display output at each cycle.                                                        
            this

        /// Get memory of the computer.
        member this.Cycle
            with get() = cycle
            