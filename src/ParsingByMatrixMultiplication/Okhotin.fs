module Okhotin

open GrammarBlock
open MatrixPerformanceBlock
open MatrixMultiplicationBlock
open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

open Alea
open Alea.CUDA
open Alea.CUDA.IL


let main (str : array<int>) (gr : BooleanRulesHolder) (S : NonTerminal) = 
    
    let nts = gr.allNonterminals
    let N = nts.Length
    let n = str.Length + 1
    let TPs = TPMatrices(gr, n)
    TPs.fill()
    TPs.fillDiagonal(str)
    
    let rec complete (l1, m1, l2, m2) = 
        if m1 - l1 > 1
        then 
            let s1 = (l1 + m1) / 2
            let s2 = (l2 + m2) / 2
            let B = l1, s1, s1, m1
            let B' = l2, s2, s2, m2
            let C = s1, m1, l2, s2
            let D = l1, s1, l2, s2
            let D' = s1, m1, s2, m2
            let E = l1, s1, s2, m2
            complete C
            TPs.chooseMultiplication(B, C, D)
            complete D
            TPs.chooseMultiplication(C, B', D')
            complete D'
            TPs.chooseMultiplication(B, D', E)
            TPs.chooseMultiplication(D, B', E)
            complete E

    let rec compute (l, m) = 
        let s = (l + m) / 2
        if m - l >= 4 
        then 
            let l = Action( fun () ->  compute (l, s) )
            let r = Action( fun () -> compute (s, m) )
            Parallel.Invoke(l, r)
        complete(l, s, s, m)

    compute(0, n) 
    TPs.finalCheck(S)


