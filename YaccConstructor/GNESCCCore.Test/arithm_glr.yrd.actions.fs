//this file was generated by GNESCC
//source grammar:../../../Tests/GNESCC/glr/arithm_glr/arithm_glr.yrd
//date:06.12.2011 22:03:00

module GNESCC.Actions_arithm_glr

open Yard.Generators.GNESCCGenerator

let getUnmatched x expectedType =
    "Unexpected type of node\nType " + x.ToString() + " is not expected in this position\n" + expectedType + " was expected." |> failwith

let value x = (x:>Lexer_arithm_glr.MyLexeme).MValue

let s0 expr = 
    let inner  = 
        match expr with
        | RESeq [x0] -> 
            let (res) =
                let yardElemAction expr = 
                    match expr with
                    | RELeaf e -> (e :?> _ ) 
                    | x -> getUnmatched x "RELeaf"

                yardElemAction(x0)
            (res)
        | x -> getUnmatched x "RESeq"
    box (inner)
let e1 expr = 
    let inner  = 
        match expr with
        | REAlt(Some(x), None) -> 
            let yardLAltAction expr = 
                match expr with
                | RESeq [x0] -> 
                    let (n) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf tNUMBER -> tNUMBER :?> 'a
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x0)
                    (value n |> int)
                | x -> getUnmatched x "RESeq"

            yardLAltAction x 
        | REAlt(None, Some(x)) -> 
            let yardRAltAction expr = 
                match expr with
                | RESeq [x0; x1; x2] -> 
                    let (l) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf e -> (e :?> _ ) 
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x0)
                    let (op) =
                        let yardElemAction expr = 
                            match expr with
                            | REAlt(Some(x), None) -> 
                                let yardLAltAction expr = 
                                    match expr with
                                    | RESeq [gnescc_x0] -> 
                                        let (gnescc_x0) =
                                            let yardElemAction expr = 
                                                match expr with
                                                | RELeaf tPLUS -> tPLUS :?> 'a
                                                | x -> getUnmatched x "RELeaf"

                                            yardElemAction(gnescc_x0)
                                        ( (+) )
                                    | x -> getUnmatched x "RESeq"

                                yardLAltAction x 
                            | REAlt(None, Some(x)) -> 
                                let yardRAltAction expr = 
                                    match expr with
                                    | REAlt(Some(x), None) -> 
                                        let yardLAltAction expr = 
                                            match expr with
                                            | RESeq [gnescc_x0] -> 
                                                let (gnescc_x0) =
                                                    let yardElemAction expr = 
                                                        match expr with
                                                        | RELeaf tMULT -> tMULT :?> 'a
                                                        | x -> getUnmatched x "RELeaf"

                                                    yardElemAction(gnescc_x0)
                                                ( ( * ) )
                                            | x -> getUnmatched x "RESeq"

                                        yardLAltAction x 
                                    | REAlt(None, Some(x)) -> 
                                        let yardRAltAction expr = 
                                            match expr with
                                            | RESeq [gnescc_x0] -> 
                                                let (gnescc_x0) =
                                                    let yardElemAction expr = 
                                                        match expr with
                                                        | RELeaf tMINUS -> tMINUS :?> 'a
                                                        | x -> getUnmatched x "RELeaf"

                                                    yardElemAction(gnescc_x0)
                                                ( (-) )
                                            | x -> getUnmatched x "RESeq"

                                        yardRAltAction x 
                                    | x -> getUnmatched x "REAlt"

                                yardRAltAction x 
                            | x -> getUnmatched x "REAlt"

                        yardElemAction(x1)
                    let (r) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf e -> (e :?> _ ) 
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x2)
                    (op l r)
                | x -> getUnmatched x "RESeq"

            yardRAltAction x 
        | x -> getUnmatched x "REAlt"
    box (inner)

let ruleToAction = dict [|(2,e1); (1,s0)|]

