namespace AoC2020.Common.Utilities

module String =

    /// Converts a string to a sequence of char.
    let toCharSeq (str : string) : char seq =
        str
        |> Seq.map (fun c -> c)

    /// Converts a string of digits to a sequence of int.
    let toIntSeq (str : string) : int seq =
        str
        |> Seq.map (string)
        |> Seq.map (fun c -> int c)