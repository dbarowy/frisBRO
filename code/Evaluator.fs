module Evaluator

open AST

let evalplayer p = 
    match p with 
    | Offense(a, b) ->
        let x = a.x |> string
        let y = a.y |> string
        " cx=\"" + x + "\" cy=\"" + y + "\""
    | Defense(a) -> 
        let x = a.x |> string
        let y = a.y |> string
        " cx=\"" + x + "\" cy=\"" + y + "\""

let rec evalplayers team = 
    match team with 
    | Offensive(ol) ->
        match ol with 
        | [] -> ""
        | o::ol -> (evalplayer o) + evalplayers(Offensive(ol))
    | Defensive(dl) ->
        match dl with 
        | [] -> ""
        | d::dl -> (evalplayer d) + evalplayers(Defensive(dl))
        


let eval (field: Team) : string =
    let width = FIELD_WIDTH |> string
    let length = FIELD_LENGTH |> string
    let team = field
    "<svg width=\"" + width + "\" height=\"" + length + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n" +
    "  <line x1=\"260\" y1=\"10\" x2=\"260\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    "  <line x1=\"960\" y1=\"10\" x2=\"960\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    "  <circle r=\"7\""+ (evalplayers team) + " fill=\"white\" />" +
    "</svg>\n"
    // "<svg width=\"300\" height=\"130\" xmlns=\"http://www.w3.org/2000/svg\"> \n<rect width=\"200\" height=\"100\" x=\"10\" y=\"10\" rx=\"20\" ry=\"20\" fill=\"blue\" /n/> /n</svg>"