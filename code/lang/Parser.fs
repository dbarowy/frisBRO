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

let force = ((pstr ", home; ")|>> (fun a -> Home)) <|> 
                    ((pstr ", away; ")|>> (fun a -> Away)) <|>
                    ((pstr ", flat; ")|>> (fun a -> Flat)) <!> "force"

let dplayer = pseq coord force (fun (a, force) -> Defense(a, force)) <!> "dplayer"

let dteam = pbetween
                (pstr "Defense = {")
                (pmany1 dplayer |>> (fun (a: Player list) -> Defensive(a)))
                (pstr "}\n")

let oplayer = pseq coord disc (fun (coord, disc) -> Offense(coord, disc)) //<!> "oplayer"

let oteam = pbetween
                (pstr "Offense = {")
                (pmany1 oplayer |>> (fun (a: Player list) -> Offensive(a)))
                (pstr "}")

// let field = pmany1 dteam |>> (fun (a) -> Grass(a))


let expr = dteam <|> oteam 

let grammar = pleft (pmany0 expr) peof

let parse (input: string) : Field option =
    let i = debug input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None