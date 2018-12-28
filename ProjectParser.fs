module ProjectParser

open Parser

// Type of game played
type GameType =
| CashGame of int*int*int // Number of Players * SB * BB
| Tournament of int*int*int
| Other

// Date * Buy-in * Cash-out * Net * Game-type * Duration in minutes * Notes
type Info = int*int*int*int*GameType*int*string

(* Parsers *)

// parses a number from digits into an int
let pnum = pmany0 pdigit |>> stringify |>> int //<!> "pnumber"

// parses a space for convenience
let pspace = pchar ' ' //<!> "pspace"

// parses a number then space
let pnumber = pleft pnum pspace //<!> "pnumber"

// parses a char then space
let pcharacter = pleft pletter pspace //<!> "pcharacter"

// 2 pnumbers
let p2 = pseq pnumber pnumber id //<!> "p2"

// 4 pnumbers
let p3 = pseq p2 p2 (fun ((a,b),(c,d)) -> (a,b,c,d)) //<!> "p3"

// 4 numbers + char
let p4 = pseq p3 pcharacter id //<!> "p4"

// 4 numbers + char + 4 numbers
let p5 = pseq p4 p3 id //<!> "p5"

// can be space or letter or number
let ppiece = pitem <|> pspace <|> pdigit //<!> "ppiece"

// formats everything
let format ((((a,b,c,d),e),(f,g,h,i)),j) : Info = 
    let gt = 
        match e with
        | 'c' -> CashGame(f,g,h)
        | 't' -> Tournament(f,g,h)
        | _ -> Other
    (a,b,c,d,gt,i,(stringify j))

// everything
let pentry = pseq p5 (pmany0 ppiece) format //<!> "pentry"

// grammar of input
let grammar = pleft pentry peof //<!> "grammar"

// parse function
let parse input : Info option=
    match grammar (prepare input) with
    | Success(e,_) -> Some e
    | Failure -> None
