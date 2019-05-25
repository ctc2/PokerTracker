(*
    All REPL related functions are kept in Library.fs. These
    algorithms are responsible for accessing/analyzing the data
    stored in the pokertracker.
*)

module Library

open System
open ProjectParser

(* TODO LIST:
    figure out how to round the inputs to only 2 decimal places
    add more functions in repl
        -most profitable number of players for cg
        -entry/date with most winnings
        -longest/biggest run of positive winnings
*)

(* Library of REPL commands *)

let inet = "\nnet = net earnings"
let igt = "\ngame = most profitable game type (Cash Game vs. Tournament)"
let ibb = "\nbestb = most profitable blinds for Cash Game in $"
let iquit = "\nquit = quits program"
let iprint = "\nprint = prints all entries"
let ibbib = "\nbestbinbb = most profitable blinds for Cash Game in Big Blinds"
let bar = "--------------------------------------\n"

// all instructions for input repl
let instructions = bar + "Input options:" + iprint + inet + igt + ibb + ibbib + iquit 

// finds net balance  
let net (data: Info list) : float =
    float (List.fold (fun a (_,_,_,b,_,_,_) -> a + b) 0.0 data)

// helper function to find CashGames/Tournaments
let find (x: char) (_,_,_,_,c,_,_) = 
    match x with 
    | 'c' -> // c for Cash Game
        match c with
        | CashGame(_,_,_) -> true
        | _ -> false
    | 't' -> // t for Tournament
        match c with
        | Tournament(_,_) -> true
        | _ -> false
    | _ -> false // Other

// gets all CashGame entries
let getcash (data: Info list) : Info list =
    List.filter (find 'c') data

// gets all Tournament entries
let gettourn (data: Info list) =
    List.filter (find 't') data

// finds most profitable game type (cg vs. t)
let gt (data: Info list) : string =
    // find net balance of each
    let netcgs = net (getcash data)
    let netts = net (gettourn data)
    let dif = netcgs - netts
    let s =
        if dif < 0.0 then
            "Tournaments are more profitable by: $" + (abs dif).ToString()
        elif dif > 0.0 then
            "Cash Games are more profitable by: $" + dif.ToString()
        else 
            "Both are equally profitable"
    s + "\nCash Games net = $" + netcgs.ToString() + ", Tournaments net = $" + netts.ToString()

// get a map of all the blinds and the net balance for each one
let rec netblinds (data: Info list) (blinds: Map<float*float,float>) : Map<float*float,float> =
    match data with
    | (_,_,_,net,CashGame(_,sb,bb),_,_) :: xs -> 
        match blinds.TryFind (sb,bb) with
        | Some t -> 
            netblinds xs (Map.add (sb,bb) (t + net) blinds)
        | None ->
            netblinds xs (Map.add (sb,bb) net blinds)
    | _ -> blinds

// finds best net balance from list
let rec findbest (blinds: ((float*float)*float) list) : (float*float)*float =
    match blinds with
    | x :: xs -> 
        let rest = findbest xs
        if (snd x) > (snd rest) then x else rest
    | [] -> (0.0,0.0),(float Int64.MinValue) 

// finds best blinds to play at in BBs
let bestblinds (data: Info list) : (float*float)*float =
    findbest (List.map (fun ((a,b),c) -> ((a,b),(c/b))) (Map.toList (netblinds (getcash data) Map.empty)))

// finds best blinds to play at in $
let bestblindsind (data: Info list) : (float*float)*float =
    findbest (Map.toList (netblinds (getcash data) Map.empty))

// prettyprints date from ddmmyy to mm/dd/yy
let ppdate d = 
        let year = d % 100
        let month = ((d % 10000) - year)/100
        let day = (d - year - month)/10000
        month.ToString() + "/" + day.ToString() + "/" + year.ToString()

// reads input and then calls function
let rec readInputs (x: int) (data: Info list) =
    if x <> 0 then
        printf "%s\n>" instructions
        let s = Console.ReadLine()
        match s with
        | "bestb" ->
            let b = bestblindsind data
            printfn "%sMost profitable blinds: $%A,$%A with net balance: $%f" 
                bar (fst (fst b)) (snd (fst b)) (snd b)
            readInputs 1 data
        | "bestbinbb" ->
            let b = bestblinds data
            printfn "%sMost profitable blinds: $%A,$%A with net balance: %f Big Blinds" 
                bar (fst (fst b)) (snd (fst b)) (snd b)
            readInputs 1 data
        | "game" ->
            printfn "%s%s" bar (gt data)
            readInputs 1 data
        | "net" -> 
            printfn "%sNet balance: $%A" bar (net data)
            readInputs 1 data
        | "print" ->
            let f = (fun (a,b,c,d,e,f,g) -> ((ppdate a),b,c,d,e,f,g))
            let s1 = "(Date, Buy-in, Cash-out, Net, GameType, Duration, [Notes])\nGameType format:\n"
            let s2 = "CashGame(No. players, SB, BB)\nTournament(No. players, Place finished)\n"
            printfn "%sAll entries: %s\n%A" bar (s1 + s2) (List.map f data)
            readInputs 1 data
        | "quit" -> 
            printf "%sQuitting program\n%s" bar bar
            readInputs 0 data
        | _ -> 
            printfn "%sNot a valid command" bar
            readInputs 1 data
    else exit 1

