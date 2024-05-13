module AST

type Coordinate = {x: int; y: int}

type HasDisc = 
| True
| False

type Flag = 
| Automatic
| Manual


type Force = 
| Home
| Away
| Flat

type Player = 
//cordinate of where is; if has disc
| Offense of Coordinate * HasDisc
| Defense of Coordinate

type Team = 
| Offensive of Player list
| Defensive of Player list

// type Plays = {prog: string}


type Field = Team list * Flag * Force 

let FIELD_WIDTH = 1100
let FIELD_LENGTH = 500
