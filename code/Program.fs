
open Evaluator
open System
open Parser

[<EntryPoint>]
let main args =
    // let file = args[0]
    // let text = IO.File.ReadAllText file
    let text = args[0]
    match parse text with
    | Some ast ->
        //printfn "%A" ast
        let svg = eval ast
        printfn "%s" svg
        0
    | None ->
        printfn "Invalid program."
        1