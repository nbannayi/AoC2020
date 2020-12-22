open AoC2020.Common.Utilities
open AoC2020.Common.Utilities.Stack
open System

// Advent of Code 2020 day 18.

type Element =    
    | Operator of char
    | OpenBracket
    | ClosedBracket
    | Number of Int64
    | Unknown

type MathsMode =
    | Basic
    | Advanced

[<EntryPoint>]
let main argv =

    let getElementType element =
        match element with
        | "(" -> OpenBracket
        | ")" -> ClosedBracket
        | "+" -> Operator('+')
        | "*" -> Operator('*')
        | _ -> let parsed, n = element |> Int64.TryParse
               if parsed then Number(n) else Unknown

    let sums =
        "InputFiles/Day18Input.txt" 
        |> Seq.ofFileLines
        |> Seq.map (fun s -> s.Replace(" ",""))
        |> Seq.map (fun s -> String.toCharSeq s |> Seq.map (fun c -> string c) |> Seq.map (getElementType >> Some))

    let rec processExpression mode expression =
        let rec processElement mode element stack =
            // Get types of elements we're currently working with.
            let prevElement = stack |> Stack.peekPrevious
            let topElement = stack |> Stack.peek
            let newElement = element
                                       
            match prevElement, topElement, newElement with
            | Some (Number a), Some (Operator o), Some (Number b) ->
                if mode = Basic || o = '+' then
                    let number = if o = '+' then a+b else a*b 
                    stack
                    |> Stack.pop |> snd |> Stack.pop |> snd  // Pop off stack twice.
                    |> Stack.push (Some (Number number))     // Replace with a op b
                else
                    stack |> Stack.push newElement                        
            | Some (Operator o1), Some (Number _), Some (Operator o2) ->
                if mode = Basic || o1 = '+' then
                    let n2 = stack |> Stack.pop |> fst
                    let n1 = stack |> Stack.pop |> snd |> Stack.pop |> snd |> Stack.pop |> fst
                    let number =
                        match n1, n2 with
                        | (Some (Number n1)), (Some (Number n2)) -> if o1 = '+' then (n1+n2) else (n1*n2)
                        | _,_ -> 0L
                    stack
                    |> Stack.pop |> snd |> Stack.pop |> snd |> Stack.pop |> snd
                    |> Stack.push (Some (Number number))
                    |> Stack.push (Some (Operator o2))
                else
                    stack |> Stack.push newElement                        
            | _, _, Some OpenBracket ->
                stack |> Stack.push (Some OpenBracket)                
            | _, _, Some ClosedBracket ->

                // Need to refactor and make immutable!!
                let mutable bracketElement = Some Unknown
                let mutable bracketStack = StackContents []
                let mutable stack' = stack

                let e, s = stack |> Stack.pop 
                bracketElement <- e
                stack' <- s

                while bracketElement <> Some OpenBracket do
                    bracketStack <- bracketStack |> Stack.push bracketElement
                    let e, s = stack' |> Stack.pop 
                    bracketElement <- e
                    stack' <- s
                //                    
                let stackResult = processExpression mode (bracketStack |> Stack.reverse |> Stack.toSeq) |> Stack.pop |> fst
                let result = match stackResult with
                             | Some (Number(result)) -> result
                             | _ -> 0L // Something went wrong!
                stack' |> Stack.push (Some(Number (result)))                   
            | _, _, _ ->
                stack |> Stack.push newElement
                   
        let mutable stack = Stack.empty
        expression |> Seq.iter (fun e -> stack <- (processElement mode e stack))

        if mode = Basic then
            if (stack |> Stack.size) > 1 then
                stack |> Stack.toSeq |> Seq.iter (fun e -> stack <- (processElement mode e stack))
        elif (stack |> Stack.size > 1) then
            if stack |> Stack.toSeq |> Seq.contains (Some (Operator('+'))) then
                stack |> Stack.toSeq |> Seq.iter (fun e -> stack <- (processElement mode e stack))               
            else
                let result =
                    stack |> Stack.toSeq |> Seq.filter (fun s -> s <> (Some (Operator('*'))))
                    |> Seq.map (fun s ->
                        match s with
                        | (Some (Number(n))) -> n
                        | _ -> 1L)
                    |> Seq.reduce (*)
                stack <- Stack.empty |> Stack.push (Some (Number(result)))                
        stack

    let processExpressionBasic expression =
        let stack = processExpression Basic expression        
        let stackResult = stack |> Stack.pop |> fst                
        match stackResult with
        | Some (Number(result)) -> result
        | _ -> 0L // Something went wrong!

    let processExpressionAdvanced expression =
        let reduceStack stack =
            let arrayStack = stack |> Stack.toSeq |> Array.ofSeq
            let reduceLength = arrayStack |> Array.findIndex (fun l -> l = (Some (Operator('+'))))                                
            arrayStack.[..(reduceLength-2)]
            |> Array.map (fun r -> match r with | Some (Number(n)) -> n | _ -> 1L)
            |> Array.reduce (*)

        let stack = processExpression Advanced expression
        let stackLength = stack |> Stack.size                
        let topElement = stack |> Stack.peek
        let prevElement = stack |> Stack.peekPrevious

        match stackLength, prevElement with
        | 1, _ -> match topElement with | Some (Number(n)) -> n | _ -> 0L
        | _, Some (Number(n)) -> match topElement with | Some (Number(n)) -> n | _ -> 0L
        | _, _ -> reduceStack stack

    let result1 = sums |> Seq.map (processExpressionBasic) |> Seq.sum
    printfn "Part 1: result is %A" result1

    let result2 = sums |> Seq.map (processExpressionAdvanced) |> Seq.sum
    printfn "Part 2: result is %A" result2    
    0