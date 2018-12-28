module Library

open System
open System.IO
open ProjectParser

// prettyprints date
let ppdate d = 
        let year = d % 100
        let month = ((d % 10000) - year)/100
        let day = (d - year - month)/10000
        month.ToString() + "/" + day.ToString() + "/" + year.ToString()

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
            | CashGame(n,sb,bb) -> "c " + n.ToString() + " " + sb.ToString() + " " + bb.ToString()
            | Tournament(n,sb,bb) -> "t " + n.ToString() + " " + sb.ToString() + " " + bb.ToString()
            | Other -> "o 0 0 0"
        date.ToString() + " " + bi.ToString() + " " + co.ToString() + " " + net.ToString() + " " + gt + " " + dur.ToString() + " [" + notes + "]"
    File.AppendAllText(filename, (formatInfo i) + "\n")

(* Library of REPL commands *)

let inet = "\nnet = displays net earnings"
let iquit = "\nquit = quits program"
let iprint = "\nprint = prints all entries"
let bar = "--------------------------------------\n"

// all instructions for input repl
let instructions = bar + "Input options:" + iprint + inet + iquit 

// finds net balance  
let net (data: Info list) =
    //type Info = int*int*int*int*GameType*int*string
    float (List.fold (fun a (_,_,_,b,_,_,_) -> a + b) 0 data) / 100.0

// need to update net function
let rec readInputs (x: int) (data: Info list) =
    if x <> 0 then
        printf "%s\n>" instructions
        let s = Console.ReadLine()
        match s with
        | "net" -> 
            printfn "%sNet balance: $%A" bar (net data)
            readInputs 1 data
        | "print" ->
            let f = (fun (a,b,c,d,e,f,g) -> ((ppdate a),b,c,d,e,f,g))
            printfn "%sAll entries:\n%A" bar (List.map f data)
            readInputs 1 data
        | "quit" -> 
            printf "%sQuitting program\n%s" bar bar
            readInputs 0 data
        | _ -> 
            printfn "%sNot an analysis command" bar
            readInputs 1 data
    else exit 1

// asks for data piece meal and returns Info type
let inputData () : Info =
    let inputData' (s: string) =
        printfn "Input %s: " s
        Console.ReadLine()
    let d = int (inputData' "date")
    let bi = int (inputData' "buy-in amount in cents")
    let co = int (inputData' "cash-out amount in cents")
    let g = List.head (Seq.toList (inputData' "c for Cash Game, t for Tournament"))
    let n = int (inputData' "number of players")
    let sb = int (inputData' "small blind in cents")
    let bb = int (inputData' "big blind in cents")
    let gt =
        match g with
        | 'c' -> CashGame(n,sb,bb)
        | 't' -> Tournament(n,sb,bb)
        | _ -> Other
    let dur = int (inputData' "duration (in mins)")
    let notes = inputData' "notes"
    d,bi,co,(co - bi),gt,dur,notes
