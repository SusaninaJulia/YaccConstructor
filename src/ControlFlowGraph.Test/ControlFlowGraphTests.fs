﻿module ControlFlowGraphTests

open NUnit.Framework

open AbstractAnalysis.Common

open ControlFlowGraph
open ControlFlowGraph.Common
open ControlFlowGraph.CfgElements
open ControlFlowGraph.InputStructures

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AbstractParser

let needPrint = false

let createEdge source target label = new ParserEdge<_>(source, target, label)

let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let tokenToPos (tokenData : _ -> obj) token = 
    let t = tokenData token
    match t with
    | :? int as i -> [i] |> Seq.ofList
    | _ -> failwith ""

[<TestFixture>]
type ``Control Flow Graph building: Simple cases``() =
    let buildAbstractAst = RNGLR.ParseExtendedCalc.buildAstAbstract
    let tokenToNumber = RNGLR.ParseExtendedCalc.tokenToNumber
    let leftSides = RNGLR.ParseExtendedCalc.leftSide
    let indToString = RNGLR.ParseExtendedCalc.numToString
    let tokenData = RNGLR.ParseExtendedCalc.tokenData

    let semicolon = RNGLR.ParseExtendedCalc.SEMICOLON 0
    let semicolonNumber = tokenToNumber semicolon
    let nodeToType = dict["assign", Assignment;]
        
    let keywordToInt = dict [SEMICOLON, semicolonNumber;]

    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let runTest graph expectedBlocksCount expectedNodesCount printNames = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            if needPrint
            then
                let astName = fst printNames
                RNGLR.ParseExtendedCalc.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealString)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            Assert.IsTrue(cfg.Entry.Parents.IsEmpty, "Entry node has parent node!")
            Assert.IsTrue(cfg.Exit.Children.IsEmpty, "Exit node has child node!")
            
            Assert.AreEqual(expectedBlocksCount, cfg.Blocks.Length, "Blocks count isn't equal expected one")
            Assert.AreEqual(expectedNodesCount, cfg.Nodes.Length, "Intermediate nodes count isn't equal expected one")
            
    [<Test>]
    member test.``Elementary test``() =
        let qGraph = new ParserInputGraph<_>(0, 13)
        let vertexRange = List.init 14 id
        qGraph.AddVertexRange vertexRange |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.ONE 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.Y 4)
                createEdge 5 6 (RNGLR.ParseExtendedCalc.EQ 5)
                createEdge 6 7 (RNGLR.ParseExtendedCalc.TWO 6)
                createEdge 7 8 (RNGLR.ParseExtendedCalc.SEMICOLON 7)
                createEdge 8 9 (RNGLR.ParseExtendedCalc.Z 8)
                createEdge 9 10 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 10 11 (RNGLR.ParseExtendedCalc.THREE 10)
                createEdge 11 12 (RNGLR.ParseExtendedCalc.SEMICOLON 11)
                createEdge 12 13 (RNGLR.ParseExtendedCalc.RNGLR_EOF 12)
            ] |> ignore

        let expectedNodes = 4
        let expectedBlocks = 3
        let printNames = "`elementary ast.dot", "`elementary cfg.dot"
        runTest qGraph expectedBlocks expectedNodes printNames

    [<Test>]
    member test.``Ambiguous test``() =
        let qGraph = new ParserInputGraph<_>(0, 16)
        let vertexRange = List.init 17 id
        qGraph.AddVertexRange vertexRange |> ignore

        //          -> Y = 2;
        // X = 1;                -> X = 4;
        //          -> Z = 3;
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.ONE 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.Y 4)
                createEdge 5 6 (RNGLR.ParseExtendedCalc.EQ 5)
                createEdge 6 7 (RNGLR.ParseExtendedCalc.TWO 6)
                createEdge 7 8 (RNGLR.ParseExtendedCalc.SEMICOLON 7)
                createEdge 4 9 (RNGLR.ParseExtendedCalc.Z 8)
                createEdge 9 10 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 10 11 (RNGLR.ParseExtendedCalc.THREE 10)
                createEdge 11 8 (RNGLR.ParseExtendedCalc.SEMICOLON 11)
                createEdge 8 12 (RNGLR.ParseExtendedCalc.X 8)
                createEdge 12 13 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 13 14 (RNGLR.ParseExtendedCalc.FOUR 10)
                createEdge 14 15 (RNGLR.ParseExtendedCalc.SEMICOLON 11)
                createEdge 15 16 (RNGLR.ParseExtendedCalc.RNGLR_EOF 12)
            ] |> ignore

        let expectedNodes = 4
        let expectedBlocks = 4
        let printNames = "`ambiguous ast.dot", "`ambiguous cfg.dot"
        runTest qGraph expectedBlocks expectedNodes printNames

    [<Test>]
    member this.``Ambiguous2 test``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        let vertexRange = List.init 6 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 0 1 (RNGLR.ParseExtendedCalc.Y 1)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 2)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.FIVE 3)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 4)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.RNGLR_EOF 5)
            ] |> ignore

        let expectedNodes = 2
        let expectedBlocks = 2
        let printNames = "`ambiguous2 ast.dot", "`ambiguous2 cfg.dot"
        runTest qGraph expectedBlocks expectedNodes  printNames

[<TestFixture>]
type ``Control Flow Graph building: Cycles``() = 
    let buildAbstractAst = RNGLR.ParseSimple.buildAstAbstract
    let tokenToNumber = RNGLR.ParseSimple.tokenToNumber
    let leftSides = RNGLR.ParseSimple.leftSide
    let indToString = RNGLR.ParseSimple.numToString
    let tokenData = RNGLR.ParseSimple.tokenData

    let semicolon = RNGLR.ParseSimple.SEMICOLON 0
    let semicolonNumber = tokenToNumber semicolon
    let nodeToType = dict["assign", Assignment;]
        
    let keywordToInt = dict [SEMICOLON, semicolonNumber;]

    let tokToRealString tok = tok |> tokenToNumber |> indToString
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let postCondition condition (exitNode : InterNode<_>) = 
        exitNode.Parents
        |> List.map(fun node -> node.Tokens |> Array.map tokenToNumber)
        |> List.forall condition

    let runTest graph expectedBlocksCount expectedNodesCount postCond printNames = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            if needPrint
            then
                let astName = fst printNames
                RNGLR.ParseSimple.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealString)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            Assert.IsTrue(cfg.Entry.Parents.IsEmpty, "Entry node has parent node!")
            Assert.IsTrue(cfg.Exit.Children.IsEmpty, "Exit node has child node!")

            Assert.AreEqual(expectedBlocksCount, cfg.Blocks.Length, "Blocks count isn't equal expected one")
            Assert.AreEqual(expectedNodesCount, cfg.Nodes.Length, "Intermediate nodes count isn't equal expected one")

            let isCorrect = postCond cfg.Exit
            Assert.IsTrue (isCorrect, "Incorrect cfg was builded")

    [<Test>]
    member this.``Cycle A+``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 3)
        let vertexRange = List.init 4 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 1 0 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.RNGLR_EOF 2)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle A+ input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 4
        let postCond = postCondition (fun _ -> true)
        let printNames = "`Cycle A+ ast.dot", "`Cycle A+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle A B*``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 4)
        let vertexRange = List.init 5 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 2 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 2 4 (RNGLR.ParseSimple.RNGLR_EOF 4)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle A B asteriks input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 2

        let expectedParents = 
            [|RNGLR.ParseSimple.A 0 ; RNGLR.ParseSimple.B 0|]
            |> Array.map tokenToNumber

        let myCond tokenSet = 
            expectedParents
            |> Array.fold (fun acc num -> acc || tokenSet |> Array.exists ((=) num)) false

        let postCond = postCondition myCond
        let printNames = "`Cycle A B asteriks ast.dot", "`Cycle A B asteriks cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle A B* C``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 6)
        let vertexRange = List.init 7 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 2 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 2 4 (RNGLR.ParseSimple.C 4)
                createEdge 4 5 (RNGLR.ParseSimple.SEMICOLON 5)
                createEdge 5 6 (RNGLR.ParseSimple.RNGLR_EOF 6)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle A B asteriks C input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 3

        let expectedParent = RNGLR.ParseSimple.C 0 |> tokenToNumber

        let myCond tokenSet = 
            tokenSet |> Array.exists ((=) expectedParent)

        let postCond = postCondition myCond
        let printNames = "`Cycle A B asteriks C ast.dot", "`Cycle A B asteriks C cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle (A | B)+``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 3)
        let vertexRange = List.init 4 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 0 1 (RNGLR.ParseSimple.B 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 1 0 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.RNGLR_EOF 2)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle (A or B)+ input.dot" tokToRealString

        let expectedNodes = 3
        let expectedBlocks = 8
        let postCond = postCondition (fun _ -> true)
        let printNames = "`Cycle (A or B)+ ast.dot", "`Cycle (A or B)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle A (B+ | C+)``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 7)
        let vertexRange = List.init 8 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 1 4 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 2 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 3 6 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseSimple.C 4)
                createEdge 5 4 (RNGLR.ParseSimple.SEMICOLON 5)
                createEdge 5 6 (RNGLR.ParseSimple.SEMICOLON 5)
                createEdge 6 7 (RNGLR.ParseSimple.RNGLR_EOF 6)
            ] |> ignore
        if needPrint 
        then qGraph.PrintToDot "`Cycle A (B+ or C+) input.dot" tokToRealString

        let expectedNodes = 4
        let expectedBlocks = 6


        let numbers = [ RNGLR.ParseSimple.B 0; RNGLR.ParseSimple.C 0] |> List.map tokenToNumber
        
        let myCond tokenSet = 
            numbers 
            |> List.fold (fun acc num -> acc || tokenSet |> Array.exists ((=) num)) false

        let postCond = postCondition myCond
        let printNames = "`Cycle A (B+ or C+) ast.dot", "`Cycle A (B+ or C+) cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames


    [<Test>]
    member this.``Cycle (AB)+``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 5)
        let vertexRange = List.init 6 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 0 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 3 4 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseSimple.RNGLR_EOF 4)
            ] |> ignore
        if needPrint 
        then qGraph.PrintToDot "`Cycle (AB)+ input.dot" tokToRealString

        let expectedNodes = 4
        let expectedBlocks = 4

        let bNumber = tokenToNumber <| RNGLR.ParseSimple.B 0

        let myCond = 
             fun tokens -> tokens |> Array.exists ((=) bNumber)

        let postCond = postCondition myCond

        let printNames = "`Cycle (AB)+ ast.dot", "`Cycle (AB)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle (AB)+C``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 7)
        let vertexRange = List.init 8 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 0 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 3 4 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseSimple.C 4)
                createEdge 5 6 (RNGLR.ParseSimple.SEMICOLON 5)
                createEdge 6 7 (RNGLR.ParseSimple.RNGLR_EOF 6)
            ] |> ignore
        if needPrint 
        then qGraph.PrintToDot "`Cycle (AB)+C input.dot" tokToRealString
        let expectedNodes = 5
        let expectedBlocks = 5

        let cNumber = tokenToNumber <| RNGLR.ParseSimple.C 0

        let postCond = postCondition (fun tokens -> tokens |> Array.exists ((=) cNumber))

        let printNames = "`Cycle (AB)+C ast.dot", "`Cycle (AB)+C cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle after cycle A+B+``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 5)
        let vertexRange = List.init 6 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 1 0 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 2 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 3 4 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseSimple.RNGLR_EOF 4)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle after cycle A+B+ input.dot" tokToRealString

        let bNumber = tokenToNumber <| RNGLR.ParseSimple.B 0

        let postCond = postCondition (fun tokens -> tokens |> Array.exists ((=) bNumber))

        let expectedNodes = 4
        let expectedBlocks = 6
        let printNames = 
            "`Cycle after cycle A+B+ ast.dot", 
            "`Cycle after cycle A+B+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle inside cycle (A+B)+``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 5)
        let vertexRange = List.init 6 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 0 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 0 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 3 4 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseSimple.RNGLR_EOF 4)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle inside cycle (A+B)+ input.dot" tokToRealString

        let bNumber = tokenToNumber <| RNGLR.ParseSimple.B 0

        let postCond = postCondition (fun tokens -> tokens |> Array.exists ((=) bNumber))

        let expectedNodes = 4
        let expectedBlocks = 6
        let printNames = 
            "`Cycle inside cycle (A+B)+ ast.dot", 
            "`Cycle inside cycle (A+B)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames

    [<Test>]
    member this.``Cycle inside cycle ((AB)+C)+``() = 
        
        let qGraph = new ParserInputGraph<_>(0, 7)
        let vertexRange = List.init 8 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseSimple.A 0)
                createEdge 1 2 (RNGLR.ParseSimple.SEMICOLON 1)
                createEdge 2 3 (RNGLR.ParseSimple.B 2)
                createEdge 3 0 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 3 4 (RNGLR.ParseSimple.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseSimple.C 4)
                createEdge 5 0 (RNGLR.ParseSimple.SEMICOLON 5)
                createEdge 5 6 (RNGLR.ParseSimple.SEMICOLON 5)
                createEdge 6 7 (RNGLR.ParseSimple.RNGLR_EOF 6)
            ] |> ignore

        if needPrint 
        then qGraph.PrintToDot "`Cycle inside cycle ((AB)+C)+ input.dot" tokToRealString

        let cNumber = tokenToNumber <| RNGLR.ParseSimple.C 0

        let postCond = postCondition (fun tokens -> tokens |> Array.exists ((=) cNumber))

        let expectedNodes = 5
        let expectedBlocks = 6
        let printNames = 
            "`Cycle inside cycle ((AB)+C)+ ast.dot", 
            "`Cycle inside cycle ((AB)+C)+ cfg.dot"
        runTest qGraph expectedBlocks expectedNodes postCond printNames


[<TestFixture>]
type ``Control Flow Graph building: If statements`` () =
    let buildAbstractAst = RNGLR.ParseIf.buildAstAbstract
    let tokenToNumber = RNGLR.ParseIf.tokenToNumber
    let leftSides = RNGLR.ParseIf.leftSide
    let indToString = RNGLR.ParseIf.numToString
    let tokenData = RNGLR.ParseIf.tokenData

    let semicolonNumber = tokenToNumber <| RNGLR.ParseIf.SEMICOLON 0
    let ifNumber = tokenToNumber <| RNGLR.ParseIf.IF 0
    let thenNumber = tokenToNumber <| RNGLR.ParseIf.THEN 0
    let elseNumber = tokenToNumber <| RNGLR.ParseIf.ELSE 0
    let endIfNumber = tokenToNumber <| RNGLR.ParseIf.ENDIF 0
    
    let nodeToType = dict[
                                "simple_statement", Assignment;
                                "if_statement", IfStatement;
                          ]
    

    let tokToRealString tok = tok |> tokenToNumber |> indToString

    let keywordToInt = dict [
                                    SEMICOLON, semicolonNumber;
                                    IF, ifNumber;
                                    THEN, thenNumber;
                                    ELSE, elseNumber;
                                    ENDIF, endIfNumber; 
                                ]

    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt)

    let runTest graph expectedBlocksCount expectedNodesCount printNames = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst graph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            
            if needPrint
            then
                let astName = fst printNames
                RNGLR.ParseIf.defaultAstToDot mAst astName
            
            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealString)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            Assert.IsTrue(cfg.Entry.Parents.IsEmpty, "Entry node has parent node!")
            Assert.IsTrue(cfg.Exit.Children.IsEmpty, "Exit node has child node!")

            Assert.AreEqual(expectedBlocksCount, cfg.Blocks.Length, "Blocks count isn't equal expected one")
            Assert.AreEqual(expectedNodesCount, cfg.Nodes.Length, "Intermediate nodes count isn't equal expected one")


    [<Test>]
    member test.``Simple If test``() =
        let qGraph = new ParserInputGraph<_>(0, 12)
        let vertexRange = List.init 13 id
        qGraph.AddVertexRange vertexRange |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseIf.IF 0)
                createEdge 1 2 (RNGLR.ParseIf.A 1)
                createEdge 2 3 (RNGLR.ParseIf.THEN 2)
                createEdge 3 4 (RNGLR.ParseIf.B 3)
                createEdge 4 5 (RNGLR.ParseIf.SEMICOLON 4)
                createEdge 5 6 (RNGLR.ParseIf.ELSE 5)
                createEdge 6 7 (RNGLR.ParseIf.C 6)
                createEdge 7 8 (RNGLR.ParseIf.SEMICOLON 7)
                createEdge 8 9 (RNGLR.ParseIf.ENDIF 8)
                createEdge 9 10 (RNGLR.ParseIf.D 9)
                createEdge 10 11 (RNGLR.ParseIf.SEMICOLON 10)
                createEdge 11 12 (RNGLR.ParseIf.RNGLR_EOF 11)
            ] |> ignore

        let printNames = "`simple if ast.dot", "`simple if cfg.dot"
        let expectedBlocksCount = 4
        let expectedNodesCount = 5
        runTest qGraph expectedBlocksCount expectedNodesCount printNames

    [<Test>]
    member test.``Big If test``() =
        let qGraph = new ParserInputGraph<_>(0, 16)
        let vertexRange = List.init 17 id
        qGraph.AddVertexRange vertexRange |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseIf.IF 0)
                createEdge 1 2 (RNGLR.ParseIf.A 1)
                createEdge 2 3 (RNGLR.ParseIf.THEN 2)
                createEdge 3 4 (RNGLR.ParseIf.B 3)
                createEdge 4 5 (RNGLR.ParseIf.SEMICOLON 4)
                createEdge 5 6 (RNGLR.ParseIf.C 5)
                createEdge 6 7 (RNGLR.ParseIf.SEMICOLON 6)
                createEdge 7 8 (RNGLR.ParseIf.ELSE 7)
                createEdge 8 9 (RNGLR.ParseIf.D 8)
                createEdge 9 10 (RNGLR.ParseIf.SEMICOLON 9)
                createEdge 10 11 (RNGLR.ParseIf.E 10)
                createEdge 11 12 (RNGLR.ParseIf.SEMICOLON 11)
                createEdge 12 13 (RNGLR.ParseIf.ENDIF 12)
                createEdge 13 14 (RNGLR.ParseIf.F 13)
                createEdge 14 15 (RNGLR.ParseIf.SEMICOLON 14)
                createEdge 15 16 (RNGLR.ParseIf.RNGLR_EOF 15)
            ] |> ignore

        let printNames = "`big if ast.dot", "`big if cfg.dot"
        let expectedBlocksCount = 6
        let expectedNodesCount = 7
        runTest qGraph expectedBlocksCount expectedNodesCount printNames

    [<Test>]
    member test.``If without else test``() =
        let qGraph = new ParserInputGraph<_>(0, 11)

        let vertexRange = List.init 12 id
        qGraph.AddVertexRange vertexRange |> ignore

        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseIf.IF 0)
                createEdge 1 2 (RNGLR.ParseIf.A 1)
                createEdge 2 3 (RNGLR.ParseIf.THEN 2)
                createEdge 3 4 (RNGLR.ParseIf.B 3)
                createEdge 4 5 (RNGLR.ParseIf.SEMICOLON 4)
                createEdge 5 6 (RNGLR.ParseIf.C 5)
                createEdge 6 7 (RNGLR.ParseIf.SEMICOLON 6)
                createEdge 7 8 (RNGLR.ParseIf.ENDIF 7)
                createEdge 8 9 (RNGLR.ParseIf.D 8)
                createEdge 9 10 (RNGLR.ParseIf.SEMICOLON 9)
                createEdge 10 11 (RNGLR.ParseIf.RNGLR_EOF 10)
            ] |> ignore

        let printNames = "`if-without-else ast.dot", "`if-without-else cfg.dot"
        let expectedBlocksCount = 4
        let expectedNodesCount = 5
        runTest qGraph expectedBlocksCount expectedNodesCount printNames
            
    [<Test>]
    member test.``Inner if``() =
        let qGraph = new ParserInputGraph<_>(0, 24)
        let vertices = Array.init 25 id
        qGraph.AddVertexRange vertices |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1  (RNGLR.ParseIf.IF 0)
                createEdge 1 2  (RNGLR.ParseIf.A 1)
                createEdge 2 3  (RNGLR.ParseIf.THEN 2)
                createEdge 3 4  (RNGLR.ParseIf.IF 3)
                createEdge 4 5  (RNGLR.ParseIf.B 4)
                createEdge 5 6  (RNGLR.ParseIf.THEN 5)
                createEdge 6 7  (RNGLR.ParseIf.C 6)
                createEdge 7 8  (RNGLR.ParseIf.SEMICOLON 7)
                createEdge 8 9  (RNGLR.ParseIf.ELSE 8)
                createEdge 9 10 (RNGLR.ParseIf.D 9)
                createEdge 10 11(RNGLR.ParseIf.SEMICOLON 10)
                createEdge 11 12(RNGLR.ParseIf.ENDIF 11)
                createEdge 12 13(RNGLR.ParseIf.ELSE 12)
                createEdge 13 14(RNGLR.ParseIf.IF 13)
                createEdge 14 15(RNGLR.ParseIf.E 14)
                createEdge 15 16(RNGLR.ParseIf.THEN 15)
                createEdge 16 17(RNGLR.ParseIf.F 16)
                createEdge 17 18(RNGLR.ParseIf.SEMICOLON 17)
                createEdge 18 19(RNGLR.ParseIf.ELSE 18)
                createEdge 19 20(RNGLR.ParseIf.G 19)
                createEdge 20 21(RNGLR.ParseIf.SEMICOLON 20)
                createEdge 21 22(RNGLR.ParseIf.ENDIF 21)
                createEdge 22 23(RNGLR.ParseIf.ENDIF 22)
                createEdge 23 24(RNGLR.ParseIf.RNGLR_EOF 23)
            ] |> ignore

        let printNames = "`inner if ast.dot", "`inner if cfg.dot"
        let expectedBlocksCount = 7
        let expectedNodesCount = 8
        runTest qGraph expectedBlocksCount expectedNodesCount printNames


[<TestFixture>]
type ``Find undefined variables`` () =
    
    let buildAbstractAst = RNGLR.ParseExtendedCalc.buildAstAbstract
    let tokenToNumber = RNGLR.ParseExtendedCalc.tokenToNumber
    let leftSides = RNGLR.ParseExtendedCalc.leftSide
    let indToString = RNGLR.ParseExtendedCalc.numToString
    let tokenData = RNGLR.ParseExtendedCalc.tokenData

    let semicolonNumber = tokenToNumber <| RNGLR.ParseExtendedCalc.SEMICOLON 0
    let eqNumber = tokenToNumber <| RNGLR.ParseExtendedCalc.EQ 0

    let nodeToType = dict["assign", Assignment;]
        
    let keywordToInt = dict [
                                SEMICOLON, semicolonNumber;
                                EQ, eqNumber;
                            ]
        
    let varsNumbers = 
        [RNGLR.ParseExtendedCalc.X 0; RNGLR.ParseExtendedCalc.Y 0; RNGLR.ParseExtendedCalc.Z 0]
        |> List.map tokenToNumber

    let isVariable tok = varsNumbers |> List.exists (fun t -> t = tok) 

    let tokToRealName = tokenToNumber >> indToString
        
    let parserSource = new CfgParserSource<_>(tokenToNumber, indToString, leftSides, tokenData)
    let langSource = new LanguageSource(nodeToType, keywordToInt, isVariable)

    let runTest qGraph expected printNames = 
        let parseResult = (new Parser<_>()).Parse buildAbstractAst qGraph
        
        match parseResult with 
        | Yard.Generators.ARNGLR.Parser.Error (num, tok, err) -> printErr (num, tok, err)
        | Yard.Generators.ARNGLR.Parser.Success (mAst) ->
            if needPrint
            then
                let astName = fst printNames
                RNGLR.ParseExtendedCalc.defaultAstToDot mAst astName

            let cfg = ControlFlow (mAst, parserSource, langSource, tokToRealName)
            
            if needPrint
            then
                let cfgName = snd printNames
                cfg.PrintToDot cfgName
            
            let errorList = cfg.FindUndefVariable()
            
            printfn "%A" errorList
            printfn "Expected: %d. Actual: %d." expected errorList.Length 
            
            Assert.AreEqual(expected, errorList.Length)

    [<Test>]
    member test.``Elementary``() = 
        let qGraph = new ParserInputGraph<_>(0, 9)
        let vertexRange = List.init 10 id
        qGraph.AddVertexRange vertexRange |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.Z 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.Y 4)
                createEdge 5 6 (RNGLR.ParseExtendedCalc.EQ 5)
                createEdge 6 7 (RNGLR.ParseExtendedCalc.X 6)
                createEdge 7 8 (RNGLR.ParseExtendedCalc.SEMICOLON 7)
                createEdge 8 9 (RNGLR.ParseExtendedCalc.RNGLR_EOF 8)
            ] |> ignore

        let expected = 1
        let printNames = "`cfg undefined variables ast elementary.dot", "`cfg undefined variables cfg elementary.dot"
        runTest qGraph expected printNames

    [<Test>]
    member test.``X = X``() = 
        let qGraph = new ParserInputGraph<_>(0, 5)
        let vertexRange = List.init 6 id
        qGraph.AddVertexRange vertexRange |> ignore
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.X 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.RNGLR_EOF 4)
            ] |> ignore

        let expected = 1
        let printNames = "`cfg undefined variables ast X = X.dot", "`cfg undefined variables cfg X = X.dot"
        runTest qGraph expected printNames

    [<Test>]
    member test.``Undef: ambiguous``() =
        let qGraph = new ParserInputGraph<_>(0, 18)
        let vertexRange = List.init 19 id
        qGraph.AddVertexRange vertexRange |> ignore

        //          -> Y = 2;
        // X = 1;                -> X = Y * Z;
        //          -> Z = 3;
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.ONE 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.Y 4)
                createEdge 5 6 (RNGLR.ParseExtendedCalc.EQ 5)
                createEdge 6 7 (RNGLR.ParseExtendedCalc.TWO 6)
                createEdge 7 8 (RNGLR.ParseExtendedCalc.SEMICOLON 7)
                createEdge 4 9 (RNGLR.ParseExtendedCalc.Z 8)
                createEdge 9 10 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 10 11 (RNGLR.ParseExtendedCalc.THREE 10)
                createEdge 11 8 (RNGLR.ParseExtendedCalc.SEMICOLON 11)
                createEdge 8 12 (RNGLR.ParseExtendedCalc.X 8)
                createEdge 12 13 (RNGLR.ParseExtendedCalc.EQ 12)
                createEdge 13 14 (RNGLR.ParseExtendedCalc.Y 13)
                createEdge 14 15 (RNGLR.ParseExtendedCalc.MULT 14)
                createEdge 15 16 (RNGLR.ParseExtendedCalc.Z 15)
                createEdge 16 17 (RNGLR.ParseExtendedCalc.SEMICOLON 16)
                createEdge 17 18 (RNGLR.ParseExtendedCalc.RNGLR_EOF 17)
            ] |> ignore

        let expected = 2
        let printNames = "`cfg undefined variables ast ambiguous1.dot", "`cfg undefined variables cfg ambiguous1.dot"
        runTest qGraph expected printNames
            
    [<Test>]
    member test.``Undef: ambiguous 2``() =
        let qGraph = new ParserInputGraph<_>(0, 15)
        let vertexRange = List.init 16 id
        qGraph.AddVertexRange vertexRange |> ignore

        //        ---> Y = 2; ---> 
        // X = 1; ---------------> X = Y * Z;
        qGraph.AddVerticesAndEdgeRange
            [
                createEdge 0 1 (RNGLR.ParseExtendedCalc.X 0)
                createEdge 1 2 (RNGLR.ParseExtendedCalc.EQ 1)
                createEdge 2 3 (RNGLR.ParseExtendedCalc.ONE 2)
                createEdge 3 4 (RNGLR.ParseExtendedCalc.SEMICOLON 3)
                createEdge 4 5 (RNGLR.ParseExtendedCalc.Y 4)
                createEdge 5 6 (RNGLR.ParseExtendedCalc.EQ 5)
                createEdge 6 7 (RNGLR.ParseExtendedCalc.TWO 6)
                createEdge 7 8 (RNGLR.ParseExtendedCalc.SEMICOLON 7)
                createEdge 4 9 (RNGLR.ParseExtendedCalc.X 8)
                createEdge 8 9 (RNGLR.ParseExtendedCalc.X 8)
                createEdge 9 10 (RNGLR.ParseExtendedCalc.EQ 9)
                createEdge 10 11 (RNGLR.ParseExtendedCalc.Y 10)
                createEdge 11 12 (RNGLR.ParseExtendedCalc.MULT 11)
                createEdge 12 13 (RNGLR.ParseExtendedCalc.Z 12)
                createEdge 13 14 (RNGLR.ParseExtendedCalc.SEMICOLON 13)
                createEdge 14 15 (RNGLR.ParseExtendedCalc.RNGLR_EOF 14)
            ] |> ignore

        let expected = 2
        let printNames = "`cfg undefined variables ast ambiguous2.dot", "`cfg undefined variables cfg ambiguous2.dot"
        runTest qGraph expected printNames

//[<EntryPoint>]
let f x = 
    let cfgBuilding = new ``Control Flow Graph building: Simple cases``()
//    cfgBuilding.``Ambiguous test``()
    let cycleBuilding = new ``Control Flow Graph building: Cycles``()
    //cycleBuilding.``Cycle A B*``()
    //cycleBuilding.``Cycle inside cycle ((AB)+C)+``()
    //cycleBuilding.``Simple Cycle``()
    //cycleBuilding.Cycle()
//    cfgBuilding.``Ambiguous test``()
//    cfgBuilding.``Ambiguous2 test``()
    let ifBuilding = new ``Control Flow Graph building: If statements``()
//    ifBuilding.``Simple If test``()
    let undefVariables = new ``Find undefined variables``()
    undefVariables.``Undef: ambiguous``()
    1