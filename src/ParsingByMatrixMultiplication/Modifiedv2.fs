module Modifiedv2

open System
open System.Collections.Generic

open GrammarBlock
open SubMatrixBlock
open SubMatrixBlock.fillLayers
open MatrixPerformanceBlock
open MatrixMultiplicationBlock


let main (str : int array) (gr : BooleanRulesHolder) (S : NonTerminal) = 
    
    let n = str.Length + 1
    let k = Math.Log(n |> float, 2.) |> int  
    let nts = gr.allNonterminals

    let layers = fill(gr, str)

    let rec completeLayer (M : Layer, el : NonTerminal) = 
        if M.[0].Length <> 1
        then
            let bottomSubLayer = M.getBottom
            completeLayer(Layer(bottomSubLayer), el)
            completeVLayer(M, el)

    and completeVLayer (M : Layer, el : NonTerminal) = 
        
        let len = M.[0].Length //в одном измерении
        let leftSubLayer = Layer(M.getLeft)
        let rightSubLayer = Layer(M.getRight)
        let topSubLayer = Layer(M.getTop)

        ()
            

//        let multTask1 = Array.map (fun el -> (leftGrounded el, rightNeighbour el, el)) leftSubLayer
//                        |> Array.append (Array.map (fun el -> (leftNeighbour el, rightGrounded el, el)) rightSubLayer)
//        let multTask2 = Array.map (fun el -> (leftGrounded el, rightNeighbour el, el)) topSubLayer
//        let multTask3 = Array.map (fun el -> (leftNeighbour el, rightGrounded el, el)) topSubLayer
//
//        Ts.chooseMultiplicationByTask(multTask1)
//        completeLayer (Array.append leftSubLayer rightSubLayer)
//        Ts.chooseMultiplicationByTask(multTask2)
//        Ts.chooseMultiplicationByTask(multTask3)
//        completeLayer (topSubLayer)
    
    let mutable i = 1   //показывает в каком слое мы находимся  
    Array.iter (fun (ly : Dictionary<NonTerminal, Layer>) -> Array.iter (fun el -> completeVLayer(ly.Item(el), el)) nts ) layers
    i <- 2        

let nts = Array.map (fun el -> NonTerminal el) [|"S";"A";"B";"BB"|]
let compr = [|nts.[0], [|nts.[1], nts.[3], true|];  // S -> A BB
              nts.[0], [|nts.[0], nts.[0], true|];  // S -> S S
              nts.[3], [|nts.[2], nts.[2], true|];  // BB -> B B
              nts.[1], [|nts.[1], nts.[1], true|];  // A -> A A
              nts.[2], [|nts.[2], nts.[2], true|]|] // B -> B B
let simpr = [|nts.[1], 0;  // A -> 0
              nts.[2], 0;  // B -> 0
              nts.[2], 1;  // B -> 1
              nts.[2], 2|] // B -> 2

let gr = BooleanRulesHolder (compr, simpr, false)

let str = [|0;0;1;0;0;1;0;0;0;0;1;0;0;1;0;0;0;0;1;0;0;1;0;0;0;0;1;0;0;1;0|]

main str gr nts.[0]


System.Console.ReadKey(true) |> ignore
