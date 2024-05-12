module Evaluator

open AST

let evalPlay force discPos = 
    match force with
    | Home -> if discPos.y < 100 then "Angled Stack"
              else if discPos.y > 300 then "Color"
              else "Vert stack"
    | Away -> if discPos.y < 100 then "Color"
              else if discPos.y > 300 then "Angled Stack"
              else "Vert stack"
    | Flat -> "Vert stack"

let evalplayer p force = 
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
        "  <circle r=\"3\" cx=\"" + x1 + "\" cy=\"" + y1 + "\" fill=\"white\" />" +
        "<text x=\"20\" y=\"480\" fill=\"black\"> Recommended Play: " + (evalPlay force a) + "</text>"
    | Defense(a, force) -> 
        let x = a.x |> string
        let y = a.y |> string
        " cx=\"" + x + "\" cy=\"" + y + "\""

let rec evalplayers team force = 
    match team with 
    | Offensive(ol) ->
        match ol with 
        | [] -> ""
        | o::ol -> "  <circle r=\"7\""+ (evalplayer o force) + (evalplayers(Offensive(ol)) force)
    | Defensive(dl) ->
        match dl with 
        | [] -> ""
        | d::dl -> "  <circle r=\"7\""+ (evalplayer d force) + " fill=\"red\" />" + (evalplayers(Defensive(dl)) force) 

let eval (field: Field) : string =
    let (teams, force) = field
    let width = FIELD_WIDTH |> string
    let length = FIELD_LENGTH |> string
    let team1 = teams[0]
    let team2 = teams[1]


    "<svg width=\"" + width + "\" height=\"" + length + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n" +
    "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    (evalplayers team1 force) + (evalplayers team2 force) +
    //"<text x=\"20\" y=\"480\" fill=\"black\"> Recommended Play: " + (evalPlay force discPos) + "</text>" +
    "</svg>\n"


    // "<svg width=\"" + width + "\" height=\"" + length + "\"" +
    // " xmlns=\"http://www.w3.org/2000/svg\"" +
    // " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    // "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n" +
    // "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    // "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" + 
    // // "  <text x=\"20\" y=\"500\" fill=\"red\">I love SVG!</text>" +
    // (evalplayers team1) + (evalplayers team2) +
    // "</svg>\n"
    // "<svg width=\"300\" height=\"130\" xmlns=\"http://www.w3.org/2000/svg\"> \n<rect width=\"200\" height=\"100\" x=\"10\" y=\"10\" rx=\"20\" ry=\"20\" fill=\"blue\" /n/> /n</svg>"