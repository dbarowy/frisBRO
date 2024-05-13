module Evaluator

open AST

let evalPlays (plays: Play list) (offense: Team) force  = 
    if plays.Length > 0 then

        let player = 
            match offense with 
            | Offensive(a) -> a[0]
            | Defensive(a) -> a[0]

        let coord = 
            match player with 
            | Offense(a, b) -> a
            | Defense(a) -> a

        
        let low, high = plays[0].range
        let playforce = plays[0].force
        if ((playforce = force) && (coord.x > low) && (coord.x < high)) then 
            "run entered play"
        else 
            "play won't work"

    else 
        "N/A"


let evalPlay force discPos = 
    match force with
    | Home -> if discPos.y < 100 then "Angled Stack"
              else if discPos.y > 300 then "Color"
              else "Vert stack"
    | Away -> if discPos.y < 100 then "Color"
              else if discPos.y > 300 then "Angled Stack"
              else "Vert stack"
    | Flat -> "Vert stack"

let evalplayer p t force = 
    match p, t with 
    | Offense(a, False), Manual ->
        let x = a.x |> string
        let y = a.y |> string
        "  <circle r=\"7\" cx=\"" + x + "\" cy=\"" + y + "\" fill=\"black\" />"
    | Offense(a, True), Manual ->
        let x = a.x |> string
        let y = a.y |> string
        let x1 = a.x+4 |> string
        let y1 = a.y+4 |> string
        "  <circle r=\"7\" cx=\"" + x + "\" cy=\"" + y + "\" fill=\"black\" />" +
        "  <circle r=\"3\" cx=\"" + x1 + "\" cy=\"" + y1 + "\" fill=\"white\" />" +
        "<text x=\"20\" y=\"460\" fill=\"black\"> Recommended Play: " + (evalPlay force a) + "</text>"
    | Defense(a), _ -> 
        let x = a.x |> string
        let y = a.y |> string
        "  <circle r=\"7\" cx=\"" + x + "\" cy=\"" + y + "\" fill=\"red\" />"
    | Offense(a, _), Automatic -> // automatic person defense (not really offense)
        let x = a.x |> string
        let y = a.y + 15 |> string
        "  <circle r=\"7\" cx=\"" + x + "\" cy=\"" + y + "\" fill=\"red\" />"

let rec evalplayers team flag force = 
    match team with 
    | Offensive(ol) ->
        match ol with 
        | [] -> ""
        | o::ol -> (evalplayer o flag force) + (evalplayers(Offensive(ol)) flag force)
    | Defensive(dl) ->
        match dl with 
        | [] -> ""
        | d::dl -> (evalplayer d flag force) + (evalplayers(Defensive(dl)) flag force) 

let eval (field: Field) : string =
    let (teams, flag, force, plays) = field
    let width = FIELD_WIDTH |> string
    let length = FIELD_LENGTH |> string
    let team1 = teams[0] // offense 
    let offense = (evalplayers team1 Manual force)

    let defense = 
        if flag = Automatic then
            (evalplayers team1 flag force)

        else 
            let team2 = teams[1]
            (evalplayers team2 flag force)

    "<svg width=\"" + width + "\" height=\"" + length + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n" +
    "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>" +
    offense + defense +
    "<text x=\"20\" y=\"480\" fill=\"black\"> User Input Play: " + (evalPlays plays team1 force) + "</text>" +
    "</svg>\n"
