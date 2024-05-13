namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Evaluator
open Parser
open Combinator

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.Basic () =
        let file = "../../../basic.txt"
        let input = IO.File.ReadAllText file
        let expected = "<svg width=\"1100\" height=\"500\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
                        "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n"+
                        "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <circle r=\"7\" cx=\"120\" cy=\"390\" fill=\"black\" />"+
                        "  <circle r=\"3\" cx=\"124\" cy=\"394\" fill=\"white\" />"+
                        "<text x=\"20\" y=\"460\" fill=\"black\"> Recommended Play: Color</text>"+
                        "  <circle r=\"7\" cx=\"140\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"160\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"180\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"200\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"240\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"120\" cy=\"370\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"140\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"160\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"180\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"200\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"240\" cy=\"200\" fill=\"red\" />"+
                        "<text x=\"20\" y=\"480\" fill=\"black\"> User Input Play: N/A</text></svg>\n"
        let resultOfParse = parse input
        match resultOfParse with
        | Some ast -> 
            let svg = eval ast
            Assert.AreEqual(expected, svg)
        | None -> Assert.IsTrue false


    [<TestMethod>]
    member this.Auto () =
        let file = "../../../auto.txt"
        let input = IO.File.ReadAllText file
        let expected = "<svg width=\"1100\" height=\"500\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
                        "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n"+
                        "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"390\" fill=\"black\" />"+
                        "  <circle r=\"3\" cx=\"224\" cy=\"394\" fill=\"white\" />"+
                        "<text x=\"20\" y=\"460\" fill=\"black\"> Recommended Play: Angled Stack</text>"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"300\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"320\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"340\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"360\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"380\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"405\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"235\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"300\" cy=\"235\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"320\" cy=\"235\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"340\" cy=\"235\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"360\" cy=\"235\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"380\" cy=\"235\" fill=\"red\" />"+
                        "<text x=\"20\" y=\"480\" fill=\"black\"> User Input Play: N/A</text></svg>\n"
        let resultOfParse = parse input
        match resultOfParse with
        | Some ast -> 
            let svg = eval ast
            Assert.AreEqual(expected, svg)
        | None -> Assert.IsTrue false


    [<TestMethod>]
    member this.AddPlay () =
        let file = "../../../addplay.txt"
        let input = IO.File.ReadAllText file
        let expected = "<svg width=\"1100\" height=\"500\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
                        "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n"+
                        "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"390\" fill=\"black\" />"+
                        "  <circle r=\"3\" cx=\"224\" cy=\"394\" fill=\"white\" />"+
                        "<text x=\"20\" y=\"460\" fill=\"black\"> Recommended Play: Color</text>"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"270\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"140\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"350\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"275\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"175\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"100\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"405\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"285\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"220\" cy=\"155\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"365\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"290\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"190\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"400\" cy=\"115\" fill=\"red\" />"+
                        "<text x=\"20\" y=\"480\" fill=\"black\"> User Input Play: run entered play</text></svg>\n"
        let resultOfParse = parse input
        match resultOfParse with
        | Some ast -> 
            let svg = eval ast
            Assert.AreEqual(expected, svg)
        | None -> Assert.IsTrue false


    [<TestMethod>]
    member this.Parse () =
        let input = "Play = if disc in range 3 to 100 and force is Force = Away\n"
        let prepped = prepare input
        match (play prepped) with
        | Success(_,_) -> Assert.IsTrue true 
        | Failure(_,_) -> Assert.IsTrue false