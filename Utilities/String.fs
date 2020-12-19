namespace AoC2020.Common.Utilities

module String =

    /// Converts a string to a sequence of char.
    let toCharSeq (str : string) : char seq =
        str
        |> Seq.map (fun c -> c)        