(*
    Runs the pokertracker program. "dotnet run in" allows user to 
    input an entry without having to parse in the data file. 
    "dotnet run" starts the REPL. 
*)

open System
open Library
open FileEditor

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
        // repl to access/use data
        readInputs 1 (readFile f)
    0
