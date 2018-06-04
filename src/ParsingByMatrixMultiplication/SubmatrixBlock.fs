module SubMatrixBlock

open System
open System.Collections.Generic

open GrammarBlock
open MatrixMultiplicationBlock

let pow2i i = 2. ** float i |> int

type SubMatrix<'T> = 
    | Sub of SubMatrix<'T> * SubMatrix<'T> * SubMatrix<'T> * SubMatrix<'T>
    | El of array<'T> // length = 4 || = 1

    member this.Length = 
        let rec length this n =
            match this with
            | El(el) -> n
            | Sub(l, _, _, _) -> length l (2 * n)
        length this 2

    member this.getLeft = 
        match this with 
        | El(el) -> failwith "can't get left"
        | Sub(l, t, b, r) -> l
    member this.getTop = 
        match this with 
        | El(el) -> failwith "can't get top"
        | Sub(l, t, b, r) -> t
    member this.getBottom = 
        match this with 
        | El(el) -> failwith "can't get bottom"
        | Sub(l, t, b, r) -> b
    member this.getRight = 
        match this with 
        | El(el) -> failwith "can't get right"
        | Sub(l, t, b, r) -> r

let rec toArray this = 
    match this with
    | El(el) -> el
    | Sub(l, t, b, r) -> Array.concat [|toArray l; toArray t; toArray b; toArray r|]

let ofArray (arr : array<'T>) = 
    let n = arr.Length
    assert (n % 4 = 0)
    let toSub = Array.init (n / 4) (fun i -> El(arr.[i * 16..i * 16 + 3]))
    let rec wrap (toSub : array<SubMatrix<'T>>) n = 
        assert (n % 4 = 0)
        match n with
        | 4 -> Sub(toSub.[0], toSub.[1], toSub.[2], toSub.[3])
        | _ -> wrap (Array.init (n / 4) (fun i -> Sub(toSub.[i * 16], toSub.[i * 16 + 1], toSub.[i * 16 + 2], toSub.[i * 16 + 3]))) (n / 16)
    wrap toSub (n / 4)

let rec createSubMatrix count (sub : SubMatrix<'T>) = // count -- number of elements
    match count with
    | 1 -> sub
    | _ -> createSubMatrix (count / 4) (Sub(sub, sub, sub, sub))

type SubMatrix<'T> with

    member this.getFullAsArray = toArray this
    member this.getLeftAsArray = toArray (this.getLeft)
    member this.getTopAsArray = toArray (this.getTop)
    member this.getBottomAsArray = toArray (this.getBottom)
    member this.getRightAsArray = toArray (this.getRight)
    member this.setLeft (sub : SubMatrix<'T>) =
        match this with 
        | El(el) -> failwith "can't set left"
        | Sub(l, t, b, r) -> Sub(sub, t, b, r)
    member this.setTop (sub : SubMatrix<'T>) =
        match this with 
        | El(el) -> failwith "can't set top"
        | Sub(l, t, b, r) -> Sub(l, sub, b, r)
    member this.setBottom (sub : SubMatrix<'T>) =
        match this with 
        | El(el) -> failwith "can't set bottom"
        | Sub(l, t, b, r) -> Sub(l, t, sub, r)
    member this.setRight (sub : SubMatrix<'T>) =
        match this with 
        | El(el) -> failwith "can't set right"
        | Sub(l, t, b, r) -> Sub(l, t, b, sub)

module fillLayers = // k -- the power of two, number of layers

    type Layer(subs : array<SubMatrix<float32>>) =     
        member this.Length = subs.Length
        member x.Item
            with get(i) = subs.[i]
            and set(i) value = subs.[i] <- value
        member this.getLeft =
            Array.map (fun (el : SubMatrix<float32>) -> el.getLeft) subs 
        member this.getTop =
            Array.map (fun (el : SubMatrix<float32>) -> el.getTop) subs 
        member this.getBottom =
            Array.map (fun (el : SubMatrix<float32>) -> el.getBottom) subs 
        member this.getRight =
            Array.map (fun (el : SubMatrix<float32>) -> el.getRight) subs 
        member this.getLeftAsArray =
            Array.map (fun (el : SubMatrix<float32>) -> el.getLeftAsArray) subs 
        member this.getTopAsArray =
            Array.map (fun (el : SubMatrix<float32>) -> el.getTopAsArray) subs 
        member this.getBottomAsArray =
            Array.map (fun (el : SubMatrix<float32>) -> el.getBottomAsArray) subs 
        member this.getRightAsArray =
            Array.map (fun (el : SubMatrix<float32>) -> el.getRightAsArray) subs 
        member this.getFullAsArray = 
            Array.map (fun (el : SubMatrix<float32>) -> el.getFullAsArray) subs 

    let createLayer i k = 
        Layer(Array.init (pow2i i - 1) (fun _ -> createSubMatrix (pow2i (k - i) * pow2i (k - i) / 4) (El([|0.0f; 0.0f; 0.0f; 0.0f|]))))

    let fill(gr : BooleanRulesHolder, str : array<int>) = 
        
        let n = str.Length + 1
        let k = Math.Log(n |> float, 2.) |> int  
        let nts = gr.allNonterminals
        let ntsPairs = gr.allNontermPairs
        let simpleHeads = gr.allHeadsOfSimpleRules
        let complexHeads = gr.allHeadsOfComplexRules
        let N = nts.Length
    
        let fillLayer1() = 
            let layer1 = new Dictionary<NonTerminal, Layer>()
            let diagonal = new Dictionary<NonTerminal, _[]>()
            let updiagonal = new Dictionary<NonTerminal, _[]>()
            Array.iter (fun el -> diagonal.Add(el, Array.init (n - 1) (fun _ -> 0.0f))
                                  updiagonal.Add(el, Array.init (n - 2) (fun _ -> 0.0f))) nts
            Array.iteri (fun l s -> Array.iter (fun el -> (diagonal.Item(el).[l] <- 1.0f)) (simpleHeads.Item(s))) str
            for i = 0 to n - 3 do
                Array.iter (fun (N1, N2) -> if diagonal.Item(N1).[i] = 1.0f && diagonal.Item(N2).[i + 1] = 1.0f
                                            then
                                                for el in complexHeads.Item(N1,N2) do
                                                    updiagonal.Item(el).[i] <- 1.0f) ntsPairs
            let getLayer1 el = 
                Layer(Array.init (pow2i (k - 1) - 1) (fun i -> El([|updiagonal.Item(el).[2 * i]; 0.0f; diagonal.Item(el).[2 * i + 1]; updiagonal.Item(el).[2 * i + 1]|])))
            Array.iter (fun el -> layer1.Add(el, getLayer1 el)) nts
            layer1
    
        let fillAllLayers() = 
            let ls = Array.init (k - 2) (fun _ -> new Dictionary<NonTerminal, Layer>())
            Array.iteri (fun i (dict : Dictionary<NonTerminal, Layer>) -> Array.iter (fun el -> dict.Add(el, createLayer (k - i - 2) k)) nts) ls
            let layers = Array.append [|fillLayer1()|] ls
            Array.iter (fun el -> for i = 1 to layers.Length - 1 do
                                      for j = 0 to layers.[i].Item(el).Length - 1 do
                                          layers.[i].Item(el).[j] <- layers.[i].Item(el).[j].setBottom(layers.[i - 1].Item(el).[2 * j + 1])) nts
            layers
        fillAllLayers()
        
module MultTasks = 
    
    open fillLayers
    
    let multTasks(M : Layer, layers : array<Dictionary<NonTerminal, Layer>>, el) =
        
        let subsCount = M.Length
        let layerNumber = Math.Log(float M.[0].Length, 2.) |> int
        let len = M.[0].Length
       
        let leftSubLayer = Layer(M.getLeft)
        let rightSubLayer = Layer(M.getRight)
        let topSubLayer = Layer(M.getTop)

        let leftSubLayerAsArray = leftSubLayer.getFullAsArray
        let rightSubLayerAsArray = rightSubLayer.getFullAsArray
        let topSubLayerAsArray = topSubLayer.getFullAsArray


        let leftGrounded(M : Layer) = 
            if layerNumber = 1
            then Layer(Array.init subsCount (fun i -> layers.[0].Item(el).[i])).getFullAsArray
            else Layer(Array.init subsCount (fun i -> layers.[layerNumber - 2].Item(el).[i])).getFullAsArray
        let rigthGrounded(M : Layer) = 
            let n = layers.[layerNumber - 2].Item(el).Length
            if layerNumber = 1
            then Layer(Array.init subsCount (fun i -> layers.[0].Item(el).[n - i - 1])).getFullAsArray
            else Layer(Array.init subsCount (fun i -> layers.[layerNumber - 2].Item(el).[n - i - 1])).getFullAsArray

        let bottomSubLayer = M.getBottomAsArray
        
        let multTask1 = Array.append (leftGrounded(leftSubLayer)) bottomSubLayer, 
                         Array.append bottomSubLayer (rigthGrounded(rightSubLayer)), 
                         Array.append leftSubLayerAsArray rightSubLayerAsArray
        let multTask2 = leftGrounded(topSubLayer), rightSubLayerAsArray, topSubLayerAsArray
        let multTask3 = leftSubLayerAsArray, rigthGrounded(topSubLayer), topSubLayerAsArray
        multTask1, multTask2, multTask3
        
    let multiplication (As, Bs, Cs) n = 
        let multmod = MatrixManagedCudaModule()
        multmod.gemmBatchedMult As Bs Cs n