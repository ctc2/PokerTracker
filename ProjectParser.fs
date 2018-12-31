(*
    Parser for reading in the file. Uses parser combinators
    from the library of parsers in Parser.fs.
*)

module ProjectParser

open Parser

// Type of game played
type GameType =
| CashGame of int*float*float // Number of Players * SB * BB
| Tournament of int*int 
| Other // for deposits/withdrawals/previous trackings

// Date * Buy-in * Cash-out * Net * Game-type * Duration in hours * Notes
type Info = int*float*float*float*GameType*float*string

(* Parser Combinators *)

// parses a positive number from digits into an int
let pposint = pmany0 pdigit |>> stringify |>> int <!> "pposint"

// parses a positive number from digits into an int
let pposint64 = pmany0 pdigit |>> stringify |>> int64 <!> "pposint64"

// takes a int * (char * int) and makes it a float
let floatify (x: int64*(char*int64)) =
    let a,(b,c) = x
    float (a.ToString() + b.ToString() + c.ToString())

// parses a positive float
let pposfloat = pseq pposint64 (pseq (pchar '.') pposint64 id) floatify <!> "pposfloat"

// parses a positive float or int
let ppos = pposfloat <|> (pposint |>> float) <!> "ppos"

// parses a negative number
let pneg = pright (pchar '-') ppos |>> (fun a -> -a) <!> "pneg"

// parses a number
let pnum = pneg <|> ppos <!> "pnum"

// parses a space for convenience
let pspace = pchar ' ' <!> "pspace"

// parses a char then space
let pcharacter = pleft pletter pspace <!> "pcharacter"

// parses a float then space
let pfs = pleft pnum pspace <!> "pfs"

// parses an int then space
let pis = pleft pposint pspace <!> "pis"

// parses a space then a float
let psf = pright pspace pnum <!> "psf"

// parses a space then an int
let psi = pright pspace pposint <!> "psi"

// parses a space then 1 int then 2 floats
let piff = pseq psi (pseq psf psf id) (fun (a,(b,c)) -> (a,b,c)) <!> "pifff"

// parses a space then 2 ints
let pii = pseq psi psi id <!> "pii"

// parses just the GameType for cash
let pcash = pbetween (pstr "(c") (pchar ')') piff |>> (fun (a,b,c) -> CashGame(a,b,c)) <!> "pcash"

// parses just the GameType for tournament
let ptourn = pbetween (pstr "(t") (pchar ')') pii |>> (fun (a,b) -> Tournament(a,b)) <!> "ptourn"

// parses just the GameType for other
let pother = (pstr "(o)") |>> (fun a -> Other) <!> "pother"

// parses the GameType
let pgametype = pcash <|> ptourn <|> pother <!> "pgametype"

// parses int then float
let pif = pseq pis pfs id <!> "pif"

// parses 2 floats
let p2f = pseq pfs pfs id <!> "pff"

// parses 1 int then 3 floats
let pi3f = pseq pif p2f (fun ((a,b),(c,d)) -> (a,b,c,d)) <!> "pifff"

// parses 1 int then 3 floats then GameType
let pi3fgt = pseq pi3f pgametype id <!> "pi3fgt"

// parses a piece (can be space or letter or number)
let ppiece = pitem <|> pspace <|> pdigit <!> "ppiece"

// parses the note
let pnote = (pmany0 ppiece) |>> stringify <!> "pnote"

// parses duration + note
let pdn = pseq (pright pspace pfs) pnote id <!> "pdn"

// parses an entire entry into an Info
let pentry : Parser<Info> = pseq pi3fgt pdn (fun (((a,b,c,d),e),(f,g)) -> a,b,c,d,e,f,g) <!> "pentry"

// grammar of input file
let grammar = pleft pentry peof <!> "grammar"

// parse function
let parse input : Info option =
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None
