﻿module Utils

module CFGUtils =
    open System.IO
    open JetBrains.ReSharper.Psi.ControlFlow.CSharp
    open JetBrains.ReSharper.Psi.ControlFlow
    open JetBrains.ReSharper.Psi.CSharp.Tree
    open JetBrains.ReSharper.Psi.Tree
    open JetBrains.ReSharper.Psi

    let private toDot (cfg: ICSharpControlFlowGraf) (outStream: StreamWriter) =
        let getNodeInfo (node: IControlFlowElement) =
            if node <> null
            then 
                let psiType = 
                    if node.SourceElement <> null 
                    then node.SourceElement.NodeType.ToString()
                    else "null"
                node.Id.ToString(), psiType
            else
                "nullnode", "nullnode"

        let printLabel (nodeNum, text) =
            outStream.WriteLine(nodeNum + " [label=\"" + nodeNum + "(" + text + ")" + "\"]")

        let printGraphNode (node: IControlFlowElement) =
            let src = getNodeInfo(node)
            printLabel src
            node.Exits
            |> List.ofSeq
            |> List.map (fun e -> e.Target)
            |> List.map 
                (
                    fun t ->
                        let target = getNodeInfo t
                        printLabel target
                        outStream.WriteLine((fst src) + " -> " + (fst target))
                )
            |> List.iter (fun edge -> outStream.WriteLine(edge))

        let rec bfs (elems: list<IControlFlowElement>) =
            match elems with
            | null :: tl ->
                outStream.WriteLine ("null_node")
                bfs tl
            | hd :: tl -> 
                printGraphNode hd
                let updatedElems =
                    tl @ (
                        hd.Exits 
                        |> List.ofSeq 
                        |> List.map (fun rib -> if rib <> null then rib.Target else null)
                    )
                bfs updatedElems
            | [] -> ()
        bfs [cfg.EntryElement]

    let cfgToDot (cfg: ICSharpControlFlowGraf) outPath name =
        use outStream = FileInfo(outPath).CreateText()
        outStream.WriteLine("digraph " + name + " {")
        toDot cfg outStream
        outStream.WriteLine("}")

    let methodToDot (methodDecl: IMethodDeclaration) =
        let methodName = methodDecl.NameIdentifier.GetText()
        let outPath = methodName + ".dot"
        let cfg = CSharpControlFlowBuilder.Build methodDecl
        cfgToDot cfg outPath methodName

    let methodsCFGToDot (file: ICSharpFile) =
        let processorAction (node: ITreeNode) = 
            match node with
            | :? IMethodDeclaration as methodDecl -> methodToDot methodDecl
            | _ -> ()
        let processor = RecursiveElementProcessor (fun node -> processorAction node)
        processor.Process file