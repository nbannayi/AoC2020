namespace AoC2020.Common.Utilities

module Timer = 

    /// General purpose timer for performance diagnostics and tuning.
    let duration message f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        printfn "   [%s] Elapsed Time: %ims" message timer.ElapsedMilliseconds
        returnValue 