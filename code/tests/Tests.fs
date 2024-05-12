namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Evaluator
open Parser

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.OffenseDefenseList () =
        let file = "/Users/skylaryarter/cs334/cs334-project-jmb13-soy1/code/tests/basic.txt"
        let input = IO.File.ReadAllText file
        let expected = "<svg width=\"1100\" height=\"500\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
                        "  <rect width=\"1200\" height=\"400\" x=\"10\" y=\"10\" fill=\"green\" />\n"+
                        "  <line x1=\"210\" y1=\"10\" x2=\"210\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <line x1=\"910\" y1=\"10\" x2=\"910\" y2=\"410\" stroke=\"white\" stroke-width=\"2\"></line>"+
                        "  <circle r=\"7\" cx=\"120\" cy=\"200\" fill=\"red\" />  <circle r=\"7\" cx=\"140\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"160\" cy=\"200\" fill=\"red\" />  <circle r=\"7\" cx=\"180\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"200\" cy=\"200\" fill=\"red\" />  <circle r=\"7\" cx=\"220\" cy=\"200\" fill=\"red\" />"+
                        "  <circle r=\"7\" cx=\"240\" cy=\"200\" fill=\"red\" />  <circle r=\"7\" cx=\"120\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"3\" cx=\"124\" cy=\"224\" fill=\"white\" />  <circle r=\"7\" cx=\"140\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"160\" cy=\"220\" fill=\"black\" />  <circle r=\"7\" cx=\"180\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"200\" cy=\"220\" fill=\"black\" />  <circle r=\"7\" cx=\"220\" cy=\"220\" fill=\"black\" />"+
                        "  <circle r=\"7\" cx=\"240\" cy=\"220\" fill=\"black\" /></svg>\n"
        let resultOfParse = parse input
        match resultOfParse with
        | Some ast -> 
            let svg = eval ast
            Assert.AreEqual(expected, svg)
        | None -> Assert.IsTrue false