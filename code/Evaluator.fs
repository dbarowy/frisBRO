module Evaluator

open AST

let evalplayer p = 
    match p with 
    | Offense(a, False) ->
        let x = a.x |> string
        let y = a.y |> string
        " cx=\"" + x + "\" cy=\"" + y + "\" fill=\"black\" />"
    | Offense(a, True) ->
        let x = a.x |> string
        let y = a.y |> string
        let x1 = a.x+4 |> string
        let y1 = a.y+4 |> string
        " cx=\"" + x + "\" cy=\"" + y + "\" fill=\"black\" />" +
        "  <circle r=\"3\" cx=\"" + x1 + "\" cy=\"" + y1 + "\" fill=\"white\" />"
    | Defense(a) -> 
        let x = a.x |> string
        let y = a.y |> string
        " cx=\"" + x + "\" cy=\"" + y + "\""

let rec evalplayers team = 
    match team with 
    | Offensive(ol) ->
        match ol with 
        | [] -> ""
        | o::ol -> "  <circle r=\"7\""+ (evalplayer o) + evalplayers(Offensive(ol))
    | Defensive(dl) ->
        match dl with 
        | [] -> ""
        | d::dl -> "  <circle r=\"7\""+ (evalplayer d) + " fill=\"red\" />" + evalplayers(Defensive(dl)) 
        


let eval (field: Field) : string =
    let width = FIELD_WIDTH |> string
    let length = FIELD_LENGTH |> string
    let team1 = field[0]
    let team2 = field[1]
    "<svg width=\"" + width + "\" height=\"" + length + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n" +
    "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    (evalplayers team1) + (evalplayers team2) +
    "</svg>\n"
    // "<svg width=\"300\" height=\"130\" xmlns=\"http://www.w3.org/2000/svg\"> \n<rect width=\"200\" height=\"100\" x=\"10\" y=\"10\" rx=\"20\" ry=\"20\" fill=\"blue\" /n/> /n</svg>"