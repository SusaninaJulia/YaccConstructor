//this file was generated by GNESCC
//source grammar:../../../Tests/GNESCC/recursive_rules/test_rec_rule_3/test_rec_rule_3.yrd
//date:06.12.2011 22:03:00

module GNESCC.Regexp_test_rec_rule_3

open Yard.Generators.GNESCCGenerator
open System.Text.RegularExpressions

let buildIndexMap kvLst =
    let ks = List.map (fun (x:string,y) -> x.Length + 2,y) kvLst
    List.fold (fun (bl,blst) (l,v) -> bl+l,((bl,v)::blst)) (0,[]) ks
    |> snd
    |> dict

let buildStr kvLst =
    let sep = ";;"
    List.map fst kvLst 
    |> String.concat sep
    |> fun s -> ";" + s + ";"

let s childsLst = 
    let str = buildStr childsLst
    let idxValMap = buildIndexMap childsLst
    let re = new Regex("((;5;)(;1;)(;5;))")
    let elts =
        let res = re.Match(str)
        if Seq.fold (&&) true [for g in res.Groups -> g.Success]
        then res.Groups
        else (new Regex("((;5;)(;1;)(;5;))",RegexOptions.RightToLeft)).Match(str).Groups
    let e2 =
        idxValMap.[elts.[4].Captures.[0].Index] |> RELeaf
    let e1 =
        idxValMap.[elts.[3].Captures.[0].Index] |> RELeaf
    let e0 =
        idxValMap.[elts.[2].Captures.[0].Index] |> RELeaf
    RESeq [e0; e1; e2]

let ruleToRegex = dict [|(1,s)|]

