﻿namespace Yard.Generators.GLL

open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR.States
open Yard.Generators.RNGLR

type Table (grammar : FinalGrammar) =   
    let getFollowSets  =  
        let nonTermsCount = grammar.indexator.nonTermCount
        let followSets = Array.create nonTermsCount Set.empty 
        followSets.[grammar.rules.startSymbol] <- Set.ofList [grammar.indexator.eofIndex]
        let wasChange = ref true
        let addElementsToSet newElements setIndex =
            let oldSet = followSets.[setIndex]
            let newSet = oldSet + newElements
            followSets.[setIndex] <- newSet
            wasChange := !wasChange || oldSet <> newSet
        while !wasChange do
            wasChange := false
            for ruleIndex in 0..grammar.rules.rulesCount-1 do            
                let currentRuleLeft = grammar.rules.leftSide ruleIndex
                let currentRuleRight = grammar.rules.rightSide ruleIndex
                let mutable previousNonTerms = []
                for symbolIndex in 0..(grammar.rules.length ruleIndex)-1 do
                    let currentSymbol = currentRuleRight.[symbolIndex]
                    List.iter (addElementsToSet grammar.firstSet.[currentSymbol]) previousNonTerms
                    if not grammar.canInferEpsilon.[currentSymbol]
                    then previousNonTerms <- []
                    if grammar.indexator.isNonTerm currentSymbol
                    then previousNonTerms <- currentSymbol::previousNonTerms
                List.iter (addElementsToSet followSets.[currentRuleLeft]) previousNonTerms
        followSets

    let follow = getFollowSets

    let getTableIndex num = 
        let mutable result = num
        if num >= grammar.indexator.termsStart && num <= grammar.indexator.termsEnd then 
            result <- num - grammar.indexator.nonTermCount
        elif num >= grammar.indexator.literalsStart && num <= grammar.indexator.literalsEnd then
            result <- num - grammar.indexator.termCount - grammar.indexator.nonTermCount
        result
    let canInferEpsilon = Array.create grammar.rules.rulesCount false
    let firstForChain = 
        let ruleCount = grammar.rules.rulesCount
        let result = Array.create ruleCount Set.empty<int>
        let mutable condition = true
        for i = 0 to ruleCount - 1 do
            let mutable condition = true
            let curRule = grammar.rules.rightSide i
            let mutable j = 0
            while condition do
                let curFirst = grammar.firstSet.[curRule.[j]]
                result.[i] <- Set.union result.[i] curFirst
                if grammar.canInferEpsilon.[curRule.[j]] then 
                    if j < curRule.Length then
                        j <- j + 1
                    else 
                    condition <- false
                    canInferEpsilon.[i] <- true
                else condition <- false
        result
            

    let _table = 
        let length1 = grammar.indexator.nonTermCount
        let length2 = grammar.indexator.fullCount - grammar.indexator.nonTermCount
        let arr = Array2D.create length1 length2 (List.empty<int>)
        let result = Array.create (length1*length2) (Array.empty<int>)
        let firsts = firstForChain
        for i = 0 to grammar.rules.rulesCount - 1 do
            let curFirst = Set.toArray firsts.[i]
            let curNTerm = grammar.rules.leftSide i
            for j = 0 to curFirst.Length - 1 do
                arr.[curNTerm, getTableIndex curFirst.[j]] <- i :: arr.[curNTerm, getTableIndex curFirst.[j]]

            if canInferEpsilon.[i] then 
                let curFollow = Set.toArray follow.[curNTerm]
                for j = 0 to curFollow.Length - 1 do
                    arr.[curNTerm, getTableIndex curFollow.[j]] <- i :: arr.[curNTerm, getTableIndex curFollow.[j]] 
                               
        for i = 0 to length1 - 1 do
            for j = 0 to length1 - 1 do
                result.[length1*i + j] <- List.toArray arr.[i,j]
        result
  
    member this.result = _table