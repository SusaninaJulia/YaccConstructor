//this file was generated by GNESCC
//source grammar:../../../Tests/GNESCC/regexp/simple/seq/seq.yrd
//date:06.12.2011 22:03:00

module GNESCC.Actions_seq

open Yard.Generators.GNESCCGenerator

let getUnmatched x expectedType =
    "Unexpected type of node\nType " + x.ToString() + " is not expected in this position\n" + expectedType + " was expected." |> failwith

let value x = (x:>Lexer_seq.MyLexeme).MValue

let s0 expr = 
    let inner  = 
        match expr with
        | RESeq [x0; gnescc_x1; x2] -> 
            let (l) =
                let yardElemAction expr = 
                    match expr with
                    | RELeaf tNUMBER -> tNUMBER :?> 'a
                    | x -> getUnmatched x "RELeaf"

                yardElemAction(x0)
            let (gnescc_x1) =
                let yardElemAction expr = 
                    match expr with
                    | RELeaf tPLUS -> tPLUS :?> 'a
                    | x -> getUnmatched x "RELeaf"

                yardElemAction(gnescc_x1)
            let (r) =
                let yardElemAction expr = 
                    match expr with
                    | RELeaf tNUMBER -> tNUMBER :?> 'a
                    | x -> getUnmatched x "RELeaf"

                yardElemAction(x2)
            ((value l |> float) + (value r |> float))
        | x -> getUnmatched x "RESeq"
    box (inner)

let ruleToAction = dict [|(1,s0)|]


//test footer

