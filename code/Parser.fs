module Parser

open AST
open Combinator
open System

(*
 *   <expr>     ::= <feild><offense><defense><disc><force><plays>
 *   <offense>  ::= <player><offense>
 *   <defense>  ::= <player><defense>
 *   <disc>     ::= <player>
 *   <defense>  ::= <player><defense>
 *   <plays>    ::= <play> | <play><plays>
 *)

let num = pmany1 pdigit |>> (fun ds -> stringify ds |> int)

let firstcoord = pbetween (pchar '(') num (pchar ',')

let secondcoord = pleft num (pchar ')')

let coord = pseq firstcoord secondcoord (fun (a, b) -> {x = a; y = b})
let dplayer = coord |>> (fun a -> Defense(a))

let dteam = pmany1 dplayer |>> (fun (a: Player list) -> Defensive(a))

// let field = pmany dteam |>> (fun (a) -> Grass(a))

let expr = dteam 

let grammar = pleft expr peof

let parse (input: string) : Team option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None