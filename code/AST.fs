module AST

type Coordinate = {x: int; y: int}

type Force = 
| Home
| Away
| Flat

type Player = 
//cordinate of where is; if has disc
| Offense of Coordinate * bool
| Defense of Coordinate 

type Team = 
| Offensive of Player list
| Defensive of Player list

type Plays = {prog: string}


type Grass = Team list

let FIELD_WIDTH = 1220
let FIELD_LENGTH = 420
