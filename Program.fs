open System
open Library

[<EntryPoint>]
let main argv =

    // file to store data
    let f = "data/data.txt"

    if argv.Length = 1 && argv.[0] = "in" then
        // input data into textfile
        let i = inputData()
        writeFile f i |> ignore
        printfn "Info added:\n%A" i 
    else 
        // repl to access use data
        readInputs 1 (readFile f)
    0
