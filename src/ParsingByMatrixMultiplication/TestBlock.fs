module TestBlock

open System

open GrammarBlock
open Okhotin
open Modified

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

let rnd = System.Random()
let str = Array.init 1023 (fun i -> rnd.Next(1))

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
printfn "%b" (Okhotin.main str gr (nts.[0]))
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
let stopWatch2 = System.Diagnostics.Stopwatch.StartNew()
printfn "%b" (Okhotin.main str gr (nts.[0]))
stopWatch.Stop()
printfn "%f" stopWatch2.Elapsed.TotalMilliseconds
let stopWatch3 = System.Diagnostics.Stopwatch.StartNew()
printfn "%b" (main str gr (nts.[0]))
stopWatch.Stop()
printfn "%f" stopWatch3.Elapsed.TotalMilliseconds
let stopWatch4 = System.Diagnostics.Stopwatch.StartNew()
printfn "%b" (main str gr (nts.[0]))
stopWatch.Stop()
printfn "%f" stopWatch4.Elapsed.TotalMilliseconds
Console.ReadKey(true) |> ignore