﻿namespace AoC2020.Common.Utilities

open System.IO

module Seq =
    
    let baseDirectory = Directory.GetParent(__SOURCE_DIRECTORY__)
   
    /// Take lines from an input file and convert to a sequence of strings.
    let ofFileLines (filePath : string) = 
        let fullPath = Path.Combine(baseDirectory.FullName, filePath)
        seq { yield! File.ReadLines fullPath }

    /// Take lines from an input file and convert to a sequence of strings chunked by a delimiter.
    let ofFileChunks (delimiter : string) (filePath : string) =                
        let fullPath = Path.Combine(baseDirectory.FullName, filePath)
        let text = File.ReadAllText fullPath
        text.Split(delimiter) |> Seq.ofArray
