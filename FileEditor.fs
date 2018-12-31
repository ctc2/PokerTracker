(*
  File Editor for pokertracker program. Deals with the reading/writing of 
  the file short of the parsing which is in ProjectParser.fs.
*)

module FileEditor

open System
open System.IO
open ProjectParser

// reads and parses a single line into an Info
let readLine (s: string) : Info =
    match (parse s) with
    | Some e -> e
    | None -> failwith "Info list not parsed!"

// reads all lines in the files and outputs Info list
let readFile (filename: string) : Info list =
    let lines = File.ReadLines(filename)
    Seq.toList (Seq.map readLine lines) 

// writes to file given filename and Info
let writeFile (filename: string) (i: Info) =
    // formats Info to string to write into file
    let formatInfo (i: Info) : string =
        let date,bi,co,net,g,dur,notes = i
        let gt =
            match g with
            | CashGame(n,sb,bb) -> "(c " + n.ToString() + " " + sb.ToString() + " " + bb.ToString() + ")"
            | Tournament(n,place) -> "(t " + n.ToString() + " " + place.ToString() + ")"
            | Other -> "(o)"
        date.ToString() + " " + bi.ToString() + " " + co.ToString() + " " + net.ToString() + " " + gt + " " + dur.ToString() + " [" + notes + "]"
    File.AppendAllText(filename, (formatInfo i) + "\n")

// asks for data piece meal and returns Info type
let inputData () : Info =
    let inputData' (s: string) =
        printfn "Input %s: " s
        Console.ReadLine()
    let d = int (inputData' "date in ddmmyy (e.g. 10/27/98 -> 271098)")
    let bi = float (inputData' "buy-in amount in $ (e.g. 10.50)")
    let co = float (inputData' "cash-out amount in $ (e.g. 15)")
    let g = List.head (Seq.toList (inputData' "c for Cash Game, t for Tournament"))
    let gt =
        match g with
        | 'c' -> 
            let n = int (inputData' "number of players")
            let sb = float (inputData' "small blind in $ (e.g. 0.1)")
            let bb = float (inputData' "big blind in $ (e.g. 0.25)")
            CashGame(n,sb,bb)
        | 't' ->
            let n = int (inputData' "number of players at beginning")
            let p = int (inputData' "place finished")
            Tournament(n,p)
        | _ -> Other
    let dur = float (inputData' "duration in hours (e.g. 1.5)")
    let notes = inputData' "notes"
    d,bi,co,(co - bi),gt,dur,notes