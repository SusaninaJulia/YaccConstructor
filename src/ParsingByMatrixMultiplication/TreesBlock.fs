module TreesBlock

open System
open System.Collections.Generic

open GrammarBlock
open MatrixMultiplicationBlock


type Bound = int * int * int * int

let len (l, m, l', m') = m - l
let size b = len b * len b

let left (l, m, l', m') = (l, (l + m)/2, l', (l' + m')/2)
let top (l, m, l', m') = (l, (l + m)/2, (l' + m')/2, m')
let bottom (l, m, l', m') = ((l + m)/2, m, l', (l' + m')/2)
let right (l, m, l', m') = ((l + m)/2, m, (l' + m')/2, m')

let rightNeighbour (l, m, l', m') = (m, 2*m - l, l', m')
let leftNeighbour (l, m, l', m') = (l, m, 2*l' - m', l')
let leftGrounded (l, m, l', m') = (l, m, m, 2 * m - l)
let rightGrounded (l, m, l', m') = (2 * l' - m', l', l', m')

let pow2i i = 2. ** float i |> int

let getBiggerBound (l, m, l', m') = 
    let ln = len (l, m, l', m') * 2
    ((l / ln) * ln, ((m + (ln-1)) / ln) * ln, (l' / ln) * ln, ((m' + (ln-1)) / ln) * ln)

type Tree<'a>  =
    | Node of (Bound * Dictionary<NonTerminal, array<'a>> ref) * array<Tree<'a>>
    | Leaf of Bound * Dictionary<NonTerminal, array<'a>> ref
    | Empty

    member this.getSizeBound = 
        match this with
        | Node((b, _), _) -> size b
        | Leaf(b, _) -> size b
        | Empty -> 0

    member this.getLeftSubTree = 
        match this with 
        | Node((_, _), arr) -> arr.[0]
        | Leaf(_, arr) -> this
        | Empty -> failwith "empty tree"
    member this.getTopSubTree = 
        match this with 
        | Node((_, _), arr) -> arr.[1]
        | Leaf(_, arr) -> this
        | Empty -> failwith "empty tree"
    member this.getBottomSubTree = 
        match this with 
        | Node((_, _), arr) -> arr.[2]
        | Leaf(_, arr) -> this
        | Empty -> failwith "empty tree"
    member this.getRightSubTree = 
        match this with 
        | Node((_, _), arr) -> arr.[3]
        | Leaf(_, arr) -> this
        | Empty -> failwith "empty tree"

    member this.getSubMatrix = 
        let rec getSubMatrix (tr : Tree<_>) = 
            match tr with 
            | Node((_, dict), _) -> dict
            | Leaf(_, dict) -> dict
            | Empty -> failwith "empty tree"
        getSubMatrix this
    
    member this.boundSearch (b : Bound) = 
        let rec search (tr : Tree<_>) = 
            match tr with 
            | Node((bnd, dict), a) -> 
                if bnd = b 
                then [|dict|]
                else Array.concat [|search a.[0]; search a.[1]; search a.[2]; search a.[3]|]
            | Leaf(bnd, dict) -> 
                if bnd = b
                then [|dict|]
                else [||]
            | Empty -> failwith "empty tree"
        (search this).[0]

    member this.setSubMatrix (arr : Dictionary<NonTerminal, array<'a>>) = 
        match this with 
        | Node((_, a), _) ->  a := arr
        | Leaf(_, a) -> a := arr
        | Empty -> failwith "empty tree"

    member this.setSubMatrixByBound (arr : Dictionary<NonTerminal, array<'a>>) (b : Bound) = 
        let rec set (tr : Tree<_>) = 
            match tr with 
            | Node((bnd, _), a) -> 
                if bnd = b 
                then tr.setSubMatrix(arr)
                else 
                    set a.[0]
                    set a.[1]
                    set a.[2]
                    set a.[3]
            | Leaf(bnd, a) -> 
                if bnd = b
                then tr.setSubMatrix(arr)
            | Empty -> failwith "empty tree"
        set this



let createTree k = 
    let pow2k = pow2i k
    let topNode = [|((0, pow2k, 0, pow2k), Array.init (pow2k * pow2k) (fun _ -> 0.0f)), [||]|]
    
    let leafs = 
        let boundEmpty = Array.init k (fun i -> Array.init (4.**(float i) |> int) (fun _ -> (0, 0, 0, 0)))
        boundEmpty.[0].[0] <- (0, pow2k, 0, pow2k)
        for i = 1 to k - 1 do
            for j = 0 to boundEmpty.[i - 1].Length - 1 do
                boundEmpty.[i].[j * 4] <- left boundEmpty.[i - 1].[j]
                boundEmpty.[i].[j * 4 + 1] <- top boundEmpty.[i - 1].[j]
                boundEmpty.[i].[j * 4 + 2] <- bottom boundEmpty.[i - 1].[j]
                boundEmpty.[i].[j * 4 + 3] <- right boundEmpty.[i - 1].[j]
        Array.init (boundEmpty.[k - 1].Length) (fun i -> Leaf(boundEmpty.[k - 1].[i], ref (new Dictionary<NonTerminal, array<_>>())))
            

    let getNextBound (tr : Tree<_>) = 
        match tr with
        | Node((b, _), _) -> getBiggerBound b
        | Leaf(b, _) -> getBiggerBound b
        | Empty -> failwith "empty tree"
    
    let getNodes (lfsArr : array<Tree<_>>) = 
        let sz = lfsArr.[0].getSizeBound
        Array.init (lfsArr.Length / 4) (fun i -> Node((getNextBound lfsArr.[i * 4], ref (new Dictionary<NonTerminal, array<_>>())), lfsArr.[i * 4..i * 4 + 3]))

    let rec createFullTree (trArr : array<Tree<_>>) = 
        match trArr.Length with
        | 1 -> trArr.[0]
        | _ -> createFullTree (getNodes trArr)

    createFullTree leafs


type TPsMatrices(gr : BooleanRulesHolder, str : array<int>) = 

    let n = str.Length + 1
    let k = Math.Log(n |> float, 2.) |> int  

    let nts = gr.allNonterminals
    let ntsPairs = gr.allNontermPairs
    let simpleHeads = gr.allHeadsOfSimpleRules
    let complexHeads = gr.allHeadsOfComplexRules
    let N = nts.Length

    let multmod = MatrixManagedCudaModule()

    let Ts = new Dictionary<NonTerminal, Tree<float32>>()

    member this.getTs() = Ts

    member this.create() = 
        for el in nts do 
            Ts.Add(el, createTree k)
    
        
        
