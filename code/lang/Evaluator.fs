module Evaluator

open AST

let doesHaveDisc a = 
    match a with 
    | Offense(_, True) -> true
    | Offense(_, False) -> false
    | Defense(_) -> false


let rec check (ast: Field) = 
    let (teams, flag, force, plays) = ast
    match teams with 
    | x::xs -> 
                match x with
                | Offensive(ls) -> match (List.filter(doesHaveDisc) ls) with 
                                                | [x] -> Some ast
                                                | _ -> None
                | Defensive(ls) -> check (xs, flag, force, plays)
    | [] -> None

let rec evalPlayList (plays: Play list) force coord = 
    match plays with 
    | x::xs ->
        let low, high = x.range
        let playforce = x.force
        if ((playforce = force) && (coord.x > low) && (coord.x < high)) then 
            " run: " + x.name + "; " + evalPlayList xs force coord
        else 
            " play " + x.name + " won't work" + "; " + evalPlayList xs force coord
    | [] -> ""
    



let evalPlays (plays: Play list) (offense: Team) force  = 
    if plays.Length > 0 then

        let player = 
            match offense with 
            | Offensive(a) -> List.filter(doesHaveDisc) a//filter function List.filter (a to bool)
            | Defensive(a) -> List.filter(doesHaveDisc) a

        let coord = 
            match player[0] with 
            | Offense(a, b) -> a
            | Defense(a) -> a

        evalPlayList plays force coord

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

let coorCheck player = 
   match player with
   | Offense(c,_) -> if c.x < 10 || c.x > 1210 || c.y < 10 || c.y > 410 then
                                    false
                                  else 
                                    true
   | Defense(c) -> if c.x < 10 || c.x > 1210 || c.y < 10 || c.y > 410 then
                                    false
                                  else 
                                    true

let rec evalplayers team flag force = 
    match team with 
    | Offensive(ol) ->
        match ol with 
        | [] -> ""
        | o::ol -> if coorCheck o then
                        (evalplayer o flag force) + (evalplayers(Offensive(ol)) flag force)
                   else 
                        printfn "Offensive Player out of bounds"
                        exit 1
    | Defensive(dl) ->
        match dl with 
        | [] -> ""
        | d::dl -> if coorCheck d then
                        (evalplayer d flag force) + (evalplayers(Defensive(dl)) flag force) 
                   else 
                        printfn "Defensive Player out of bounds"
                        exit 1


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
    "<text x=\"20\" y=\"480\" fill=\"black\"> User Input Plays: " + (evalPlays plays team1 force) + "</text>" +
    "</svg>\n"
