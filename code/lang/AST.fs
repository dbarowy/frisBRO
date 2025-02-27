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

type Play = {range: int * int; force: Force; name: string}

type Player = 

| Offense of Coordinate * HasDisc
| Defense of Coordinate

type Team = 
| Offensive of Player list
| Defensive of Player list

type Field = Team list * Flag * Force * Play list

let FIELD_WIDTH = 1100
let FIELD_LENGTH = 500
