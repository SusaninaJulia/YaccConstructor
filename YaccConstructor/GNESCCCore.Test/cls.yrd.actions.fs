//this file was generated by GNESCC
//source grammar:../../../Tests/GNESCC/regexp/simple/cls/cls.yrd
//date:12/7/2011 11:30:10

module GNESCC.Actions_cls

open Yard.Generators.GNESCCGenerator

let getUnmatched x expectedType =
    "Unexpected type of node\nType " + x.ToString() + " is not expected in this position\n" + expectedType + " was expected." |> failwith

let value x = (x:>Lexer_cls.MyLexeme).MValue

let s0 expr = 
    let inner  = 
        match expr with
        | RESeq [x0] -> 
            let (lst) =
                let yardElemAction expr = 
                    match expr with
                    | REClosure(lst) -> 
                        let yardClsAction expr = 
                            match expr with
                            | RESeq [x0] -> 
                                let (m) =
                                    let yardElemAction expr = 
                                        match expr with
                                        | RELeaf tMULT -> tMULT :?> 'a
                                        | x -> getUnmatched x "RELeaf"

                                    yardElemAction(x0)
                                (m)
                            | x -> getUnmatched x "RESeq"

                        List.map yardClsAction lst 
                    | x -> getUnmatched x "REClosure"

                yardElemAction(x0)
            (List.map value lst |> String.concat ";")
        | x -> getUnmatched x "RESeq"
    box (inner)

let ruleToAction = dict [|(1,s0)|]

