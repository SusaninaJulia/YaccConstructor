//this tables was generated by GNESCC
//source grammar:../../../Tests/GNESCC/glr/arithm_glr/arithm_glr.yrd
//date:06.12.2011 22:03:00

module Yard.Generators.GNESCCGenerator.Tables_arithm_glr

open Yard.Generators.GNESCCGenerator
open Yard.Generators.GNESCCGenerator.CommonTypes

type symbol =
    | T_MINUS
    | T_MULT
    | T_PLUS
    | T_NUMBER
    | NT_e
    | NT_s
    | NT_gnesccStart
let getTag smb =
    match smb with
    | T_MINUS -> 9
    | T_MULT -> 8
    | T_PLUS -> 7
    | T_NUMBER -> 6
    | NT_e -> 5
    | NT_s -> 4
    | NT_gnesccStart -> 2
let getName tag =
    match tag with
    | 9 -> T_MINUS
    | 8 -> T_MULT
    | 7 -> T_PLUS
    | 6 -> T_NUMBER
    | 5 -> NT_e
    | 4 -> NT_s
    | 2 -> NT_gnesccStart
    | _ -> failwith "getName: bad tag."
let prodToNTerm = 
  [| 2; 1; 0 |];
let symbolIdx = 
  [| 4; 5; 2; 3; 1; 0; 3; 2; 1; 0 |];
let startKernelIdxs =  [0]
let isStart =
  [| [| true; true; true |];
     [| false; false; false |];
     [| false; false; false |];
     [| false; false; false |];
     [| false; false; false |];
     [| false; false; true |];
     [| false; false; true |];
     [| false; false; true |]; |]
let gotoTable =
  [| [| Some 2; Some 1; None |];
     [| None; None; None |];
     [| None; None; None |];
     [| None; None; None |];
     [| None; None; None |];
     [| Some 3; None; None |];
     [| Some 3; None; None |];
     [| Some 3; None; None |]; |]
let actionTable = 
  [| [| [Error]; [Error]; [Error]; [Shift 4]; [Error]; [Error] |];
     [| [Accept]; [Accept]; [Accept]; [Accept]; [Accept]; [Accept] |];
     [| [Shift 7]; [Shift 6]; [Shift 5]; [Reduce 1]; [Reduce 1]; [Reduce 1] |];
     [| [Reduce 2; Shift 7]; [Reduce 2; Shift 6]; [Reduce 2; Shift 5]; [Reduce 2]; [Reduce 2]; [Reduce 2] |];
     [| [Reduce 2]; [Reduce 2]; [Reduce 2]; [Reduce 2]; [Reduce 2]; [Reduce 2] |];
     [| [Error]; [Error]; [Error]; [Shift 4]; [Error]; [Error] |];
     [| [Error]; [Error]; [Error]; [Shift 4]; [Error]; [Error] |];
     [| [Error]; [Error]; [Error]; [Shift 4]; [Error]; [Error] |]; |]
let tables = 
  {StartIdx=startKernelIdxs
   SymbolIdx=symbolIdx
   GotoTable=gotoTable
   ActionTable=actionTable
   IsStart=isStart
   ProdToNTerm=prodToNTerm}
