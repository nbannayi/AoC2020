open AoC2020.Common.Utilities

// Advent of Code 2020 day 12.

open System

[<EntryPoint>]
let main argv =

    let bearings =
        "InputFiles/Day12Input.txt" 
        |> Seq.ofFileLines
        |> Seq.map (fun b -> (b.[0], int(b.[1..])))

    let getManhattanDistance position =
        let (x, y, _) = position
        ((double x |> Math.Abs) + (double y |> Math.Abs)) |> int

    let getNewPosition position bearing =

        let translate p m =
            let (x,y,d) = p
            match d with | 'N' -> (x, y-m) | 'S' -> (x, y+m) | 'E' -> (x+m, y) | 'W' -> (x-m, y) | _ -> (x,y)

        let rotate d a lr =
            let i = if lr = 'R' then 0 else 1
            match a with
            | 90 -> match d with | 'N' -> "EW".[i] | 'W' -> "NS".[i] | 'S' -> "WE".[i] | 'E' -> "SN".[i] | _ -> d
            | 180 -> match d with | 'N' -> 'S' | 'S' -> 'N' | 'E' -> 'W' | 'W' -> 'E' | _ -> d
            | 270 -> match d with | 'N' -> "WE".[i] | 'W' -> "SN".[i] | 'S' -> "EW".[i] | 'E' -> "NS".[i] | _ -> d
            | _ -> d

        let (x,y,d) = position
        match bearing with
        | ('N', m) -> (x, y-m, d)
        | ('S', m) -> (x, y+m, d)
        | ('E', m) -> (x+m, y, d)
        | ('W', m) -> (x-m, y, d)
        | ('R', m) -> (x, y, rotate d m 'R')
        | ('L', m) -> (x, y, rotate d m 'L')
        | ('F', m) -> let x, y = (translate position m) 
                      (x, y, d)
        | (_,_) -> position

    let getNewPositionWithWaypoint (position, waypoint) bearing = 

        let translate p m w =
            let (x,y,d) = p            
            ((x + m * fst w, y + m * snd w, d),waypoint)

        let rotate position w a lr =
            let (xw,yw) = w
            let sign = if lr = 'R' then 1 else -1

            match a with
            | 90 -> let w' = (sign * yw, -sign * xw)
                    (position, w')
            | 180 -> let w' = (yw, -xw)
                     let (xw', yw') = w'
                     (position,(yw', -xw'))               
            | 270 -> let w' = (-sign * yw, sign * xw)
                     (position, w')   
            | _ -> (position, (xw, yw))

        let (d, m) = bearing               
        let (xw, yw) = waypoint                      

        match bearing with
        | ('F', m) -> translate position m waypoint
        | ('L', m) -> rotate position waypoint m 'L'
        | ('R', m) -> rotate position waypoint m 'R'
        | ('N', m) -> (position, (xw, yw+m))
        | ('S', m) -> (position, (xw, yw-m))
        | ('E', m) -> (position, (xw+m, yw))
        | ('W', m) -> (position, (xw-m, yw))
        | _, _ -> (position, waypoint)
           
    let finalPosition =
        bearings
        |> Seq.fold (fun acc elem -> getNewPosition acc elem) (0,0,'E')

    printfn "Part 1: result is %d" (getManhattanDistance finalPosition)
    
    let finalPositionWithWaypoint =
        bearings
        |> Seq.fold (fun acc elem -> getNewPositionWithWaypoint acc elem) ((0,0,'E'), (10,1))
        |> fst

    printfn "Part 2: result is %d" (getManhattanDistance finalPositionWithWaypoint)
    0

