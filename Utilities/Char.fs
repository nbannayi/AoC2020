namespace AoC2020.Common.Utilities

module Char =

    /// Converts a char literal to an int.
    let toInt (ch : char) : int option =
        match ch with
        |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> ch |> (string >> int) |> Some
        | _ -> None        
        