module Parser

open AST
open Combinator
open System

(*
 *   <expr>     ::= <offense><defense>
 *   <offense>  ::= <oplayer><offense>
 *   <defense>  ::= <dplayer><defense>
 *  <oplayer> 	::= <coordinate><disc>
 *  <dplayer>	::= <coordinate>
 *   <disc>     ::= true | false
 *  <coordinate>::= (<digit>,<digit>)
 *      <n>		::= <digit>n | n where n is any positive integer

 *)

let num = pmany1 pdigit |>> (fun ds -> stringify ds |> int)

let firstcoord = pbetween (pchar '(') num (pchar ',')

let secondcoord = pleft num (pchar ')')

let coord = pseq firstcoord secondcoord (fun (a, b) -> {x = a; y = b})

let disc = ((pstr ", true; ") |>> (fun a -> True)) <|> ((pstr ", false; ") |>> (fun b -> False)) //<!> "disc"


let dplayer = pleft coord (pstr "; ") |>> (fun (a) -> Defense(a)) <!> "dplayer"

let persondefense  = []

let dteam = pbetween
                (pstr "Defense = {")
                (pmany1 dplayer |>> (fun (a: Player list) -> Defensive(a)))
                (pstr "}\n")

let oplayer = pseq coord disc (fun (coord, disc) -> Offense(coord, disc)) //<!> "oplayer"

let oteam = pbetween
                (pstr "Offense = {")
                (pmany1 oplayer |>> (fun (a: Player list) -> Offensive(a)))
                (pstr "}\n")

let newForce = pbetween (pstr "Force = ") (pstr "Home" <|> pstr "Away" <|> pstr "Flat") (pstr "\n") 
                    |>> (fun force -> match force with
                                                    | "Home" -> Home
                                                    | "Away" -> Away
                                                    | "Flat" -> Flat
                                                    | _ -> exit 1)

let expr = dteam <|> oteam

let flag = (pstr "Defense = Automatic\n" |>> (fun auto -> Automatic)) <|>
            (pstr "Defense = Manual\n" |>> (fun auto -> Manual))
//let flagforce = pseq flag newForce (fun (a, b) -> (a,b))

let flagforce = pseq flag newForce (fun (a, b) -> (a,b))

let firstrange = (pbetween (pstr "Play = if disc in range ") num (pstr " to "))

let range = pseq firstrange num (fun(a, b) -> (a, b))

let name = pbetween (pstr " with name as ") (pmany1 pletter |>> stringify) (pstr "\n")<!> "name"

let forceName = pseq newForce name (fun a -> (a)) <!> "forcename"

let play = pseq range (pright (pstr " and force is ") forceName) (fun(a, (b, c)) -> {range = a; force = b; name = c})

let flagforceplay = pseq flagforce (pmany0 play) (fun ((a, b), c) -> (a, b, c))

//let field = pseq (pmany0 expr) (flagforce) (fun (a, (b, c)) -> (a,b,c))

let field = pseq (pmany0 expr) (flagforceplay) (fun (a, (b, c, d)) -> (a,b,c,d))

let grammar = pleft (field) peof

let parse (input: string) : Field option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None