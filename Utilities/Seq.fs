namespace AoC2020.Common.Utilities

open System.IO

module Seq =

    /// Take lines from an input file and convert to a sequence of strings.
    let ofFile (filePath : string) = 

        let baseDirectory = Directory.GetParent(__SOURCE_DIRECTORY__)
        let fullPath = Path.Combine(baseDirectory.FullName, filePath)

        seq { yield! File.ReadLines fullPath }
