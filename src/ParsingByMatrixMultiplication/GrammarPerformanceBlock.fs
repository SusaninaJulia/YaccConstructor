module GrammarPerformanceBlock

open System
open System.IO
open GrammarBlock

let toBooleanHolder (path : string) = //for CFG
    let txt = IO.File.ReadAllLines(path)
    let rls = Array.map (fun (el : string) -> el.Split(' ')) txt
    printfn "%A" rls
    let cxrules = 
        Array.filter (fun (el : array<_>) -> el.Length = 4) rls
        |> Array.map (fun el -> NonTerminal(el.[0]), ([|NonTerminal(el.[2]), NonTerminal(el.[3]), true|]))
    let smprules = Array.filter (fun (el : array<_>) -> el.Length = 3) rls
    let tokens = 
        Array.map (fun (el : array<_>) -> el.[2]) smprules
        |> Array.distinct
    let smprules2 = Array.map (fun (el : array<_>) -> NonTerminal(el.[0]), (Array.IndexOf(tokens, el.[2]))) smprules
    cxrules, smprules2