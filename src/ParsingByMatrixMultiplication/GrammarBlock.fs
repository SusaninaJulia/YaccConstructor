module GrammarBlock

open System
open System.Collections.Generic

type NonTerminal = NonTerminal of string

type BooleanRulesHolder(complexRules: ((NonTerminal) * (NonTerminal * NonTerminal * bool) []) [],
                        simpleRules: (NonTerminal * int) [],
                        epsilonRule: bool) =  
                                         
    member this.allSimpleRules = simpleRules
    member this.allComplexRules = complexRules
    member this.isEplisonExist = epsilonRule

    member this.isCFG = 
        let f (a, b, c) = (c = true) 
        if Array.forall 
            (fun (el : (NonTerminal) * (NonTerminal * NonTerminal * bool) []) -> Array.length(snd el) = 1 && f (snd el).[0]) 
            complexRules
        then true
        else false

    member this.allTerminals = 
        Array.map (fun (_, i) -> i) simpleRules 
        |> Array.distinct

    member this.allNonterminals = 
        Array.map (fun (nt, _) -> nt) complexRules 
        |> Array.append (Array.map (fun (nt, _) -> nt) simpleRules) 
        |> Array.distinct
    
    member this.allNontermPairs = 
        Array.map (fun (_, nn) -> nn) complexRules
        |> Array.concat
        |> Array.map (fun (N1, N2, b) -> (N1, N2))
        |> Array.distinct
    
    member this.headsOfSimpleRules(token) =
        Array.filter (fun (_,t) -> t = token) simpleRules 
        |> Array.map (fun (nont,_) -> nont) 
        |> Array.distinct

    member this.allHeadsOfSimpleRules =
        let tokens = this.allTerminals
        let res = new Dictionary<int, NonTerminal[]>()
        for i = 0 to tokens.Length - 1 do 
            res.Add(tokens.[i], this.headsOfSimpleRules tokens.[i])
        res

    member this.headsOfComplexRules(N1, N2) = //for CFG
        Array.filter (fun (n, arr : _ []) -> arr.[0] = (N1, N2, true)) complexRules
        |> Array.map (fun (N3, _) -> N3)

    member this.allHeadsOfComplexRules = //for CFG
        let pairs = this.allNontermPairs
        let res = new Dictionary<NonTerminal * NonTerminal, NonTerminal[]>()
        for i = 0 to pairs.Length - 1 do 
            res.Add(pairs.[i], this.headsOfComplexRules pairs.[i])
        res

