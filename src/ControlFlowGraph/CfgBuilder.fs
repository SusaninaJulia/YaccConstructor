﻿module ControlFlowGraph.CfgBuilder

open System.Collections.Generic

open ControlFlowGraph.AssignmentHelper
open ControlFlowGraph.Common
open ControlFlowGraph.GraphInterpreter
open ControlFlowGraph.IfHelper
open ControlFlowGraph.InnerGraph
open ControlFlowGraph.InputStructures
open ControlFlowGraph.TokensExtractor

open SeqExtension

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode


let buildCfg (tree : Tree<'TokenType>) 
            (parserSource : CfgParserSource<'TokenType>) 
            (langSource : LanguageSource) 
            tokToString = 

    let intToToken i = tree.Tokens.[i]
    
    let processIf' = processIf intToToken parserSource.TokenToNumber tokToString <| langSource.GetTempIfDict()
    let processAssignment' = processAssignment intToToken tokToString 
    
    let familyToState = new Dictionary<_, ASTProcessingState>()

    //hack against duplicated epsilon edges
    let addEpsilonEdge = 
        let epsEdges = new ResizeArray<_>()
        fun (graph : BlocksGraphBuilder<_>) source target ->
            if source <> target 
            then 
                let epsilon = (source, target)
                if not <| epsEdges.Contains epsilon
                then
                    graph.AddEdgeFromTo EmptyEdge source target
                    epsEdges.Add epsilon

    let rec handleNode (graph : BlocksGraphBuilder<_>) (node : obj) = 
        let handleFamily startVertex (family : Family) =
            graph.CurrentVertex <- startVertex
            
            match familyToState.TryGetValue family with
            | true, state ->
            
                match state with
                | Processed (source, target) ->                     
                    addEpsilonEdge graph graph.CurrentVertex source
                    
                    graph.CurrentVertex <- target
                    graph.NextVertex <- target
                    Some target
                | InProgress start -> 
                    addEpsilonEdge graph startVertex start
                    
                    match graph.TryFindLastVertex start with
                    | Some vertex ->  
                        familyToState.[family] <- Processed(start, vertex)
                        graph.CurrentVertex <- vertex
                        graph.NextVertex <- vertex
                        Some vertex
                    | None -> None
            | false, _ -> 
                familyToState.[family] <- InProgress(graph.CurrentVertex)
                let familyName = parserSource.LeftSides.[family.prod] |> parserSource.NumToString

                if langSource.NodeToType.ContainsKey familyName 
                then 
                    let edge = 
                        match langSource.NodeToType.[familyName] with
                        | IfStatement -> processIf' handleNode family
                        | Assignment -> processAssignment' family
                        | x -> invalidOp <| sprintf "This construction isn't supported now: %A" x
                    
                    graph.AddEdge edge
                    graph.UpdateVertex()
                else 
                    family.nodes.doForAll (handleNode graph)
                
                familyToState.[family] <- Processed(startVertex, graph.CurrentVertex)
                Some graph.CurrentVertex

        match node with
        | :? Epsilon 
        | :? Terminal -> ()
        | :? AST as ast ->
            let commonStart = graph.CurrentVertex
            let endNumbers = 
                ast.map (handleFamily commonStart)
                |> Seq.distinct
                |> Seq.filter Option.isSome 
                |> Seq.map Option.get
                
            match Seq.length endNumbers with
            | 0 -> ()
            | 1 -> graph.UpdateVertex()
            | _ -> 
                let commonEndVertex = graph.CreateNewVertex()

                endNumbers
                |> Seq.iter(fun num -> addEpsilonEdge graph num commonEndVertex)
                graph.UpdateVertex()
                    
        | x -> failwithf "Unexpected node type: %A" x

    let graphInfo = new BlocksGraphBuilder<_>()
    handleNode graphInfo tree.Root 

    match graphInfo.TryFindLastVertex graphInfo.CurrentVertex with
    | Some _ -> ()
    | None -> graphInfo.AddEdge EmptyEdge

    graphToCfg <| graphInfo.Build() <| Some parserSource.TokenToString