module MatrixPerformanceBlock

open GrammarBlock
open MatrixMultiplicationBlock

open System
open System.Collections.Generic

open Alea
open Alea.CUDA
open Alea.CUDA.IL

type Bounds(l1, m1, l2, m2 : int) =  

    member this.getTop = l1, l2
    member this.getLength = m1 - l1


type TPMatrices(gr : BooleanRulesHolder, n) =  
     
     let fullSize = n
     let Ts = new Dictionary<NonTerminal, _[]>()

     let target = GPUModuleTarget.DefaultWorker
     let multmod = new MatrixMultiplyModule(target)
     let multmod2 = MatrixManagedCudaModule()

     let nts = gr.allNonterminals
     let ntsPairs = gr.allNontermPairs
     let simpleHeads = gr.allHeadsOfSimpleRules
     let complexHeads = gr.allHeadsOfComplexRules

     member this.getTs = Ts
             
     member this.fill(fullSize)= 
        for i = 0  to nts.Length - 1 do
            Ts.Add(nts.[i], Array.init (n * n) (fun _ -> 0.0f))
     
//     member this.fillByToken(strEl, number) =
//        Array.iter (fun el -> (Ts.Item(el).[number * fullSize + number + 1] <- 1.0f)) (simpleHeads.Item(strEl)) 

     member this.fillDiagonal(str : int[]) = 
        Array.iteri (fun l s -> Array.iter (fun el -> (Ts.Item(el).[l * fullSize + l + 1] <- 1.0f)) (simpleHeads.Item(s))) str

     member this.getSub(nt, b1 : Bounds) = 
        let n = b1.getLength
        let arr = Array.init (n * n) (fun _ -> 0.0f)
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do 
                arr.[i * n + j] <- Ts.Item(nt).[(fst b1.getTop + i) * fullSize + (snd b1.getTop + j)]
        arr

     member this.getAllSubs(b1 : Bounds) = 
        let A = new ResizeArray<_>()
        for (N1, N2) in ntsPairs do
            A.Add(this.getSub(N1, b1))
        A.ToArray()

     member this.setSub(ntPair, len, topX, topY, arr : array<_>) =
        for i = 0 to len - 1 do
            for j = 0 to len - 1 do
                if arr.[i * len + j] > 0.0f
                then
                    for el in complexHeads.Item(ntPair) do
                        Ts.Item(el).[(topX + i) * fullSize + (topY + j)] <- 1.0f
     
     member this.setAllSubs(b3 : Bounds, arrs : array<_>) = 
        let len = b3.getLength
        let topX, topY = b3.getTop 
        for i = 0 to arrs.Length - 1 do
            this.setSub(ntsPairs.[i], len, topX, topY, arrs.[i])

     member this.setAllSubsByTask (task : array<_>) (Cs : array<_>) = 
        let taskLen = task.Length
        let lenForTask = Cs.Length / taskLen
        let f (a, b, c) = Bounds(c)
        for i = 0 to taskLen - 1 do
            this.setAllSubs((f task.[i]), Cs.[i * lenForTask .. (i + 1) * lenForTask - 1])

     member this.simpleMultiplication(N1, b1 : Bounds, N2, b2 : Bounds, b3 : Bounds) = 
        let iter = b1.getLength
        for i = 0 to iter - 1 do 
            for j = 0 to iter - 1 do
                for k = 0 to iter - 1 do
                    if Ts.Item(N1).[(fst b1.getTop + i) * fullSize + (snd b1.getTop + j)]  > 0.0f && Ts.Item(N2).[(fst b2.getTop + i) * fullSize + (snd b2.getTop + j)]  > 0.0f
                    then 
                        for el in complexHeads.Item((N1, N2)) do
                            Ts.Item(el).[(fst b3.getTop + i) * fullSize + (snd b3.getTop + j)] <- 1.0f

     member this.cudaMultiplication(N1, b1 : Bounds, N2, b2 : Bounds, b3 : Bounds) = 
        let iter = b1.getLength        
        for el in complexHeads.Item((N1, N2)) do
            let A = this.getSub(N1, b1)
            let B = this.getSub(N2, b2)
            Ts.Item(el) <- multmod.Mult(A, B, Ts.Item(el), fullSize, iter, 32, 0, 0, 0, 0, fst b3.getTop, snd b3.getTop)
            
     member this.allSimpleMultiplication(b1 : Bounds, b2 : Bounds, b3 : Bounds) = 
        Array.iter (fun (N1, N2) -> this.simpleMultiplication(N1, b1, N2, b2, b3)) ntsPairs

     member this.allSimpleMultiplicationByTask(task : array<_>, isOne : bool) = 
        let toBounds (a, b, c) = Bounds(a), Bounds(b), Bounds(c)     
        if isOne 
        then Array.Parallel.iter (fun el -> this.allSimpleMultiplication(toBounds(el))) task
        else Array.iter (fun el -> this.allSimpleMultiplication(toBounds(el))) task

     member this.allCudaMultiplication(b1, b2, b3) = 
        Array.iter (fun (N1, N2) -> this.cudaMultiplication(N1, b1, N2, b2, b3)) ntsPairs

     member this.gemmMultiplication(b1 : Bounds, b2 : Bounds, b3 : Bounds) = 
        let (As, Bs, Cs) = this.getAllSubs(b1), this.getAllSubs(b2), this.getAllSubs(b3)
        multmod2.gemmBatchedMult As Bs Cs b1.getLength
        this.setAllSubs(b3, Cs)
 
     member this.gemmMultiplicationByTask(task : array<_>) =
        let toBounds (a, b, c) = Bounds(a), Bounds(b), Bounds(c)
        let As = new ResizeArray<_>()
        let Bs = new ResizeArray<_>()
        let Cs = new ResizeArray<_>()
        let mutable n = 0
        for i = 0 to task.Length - 1 do
            let (b1, b2, b3) = toBounds(task.[i])
            n <- b1.getLength
            As.Add(this.getAllSubs(b1))
            Bs.Add(this.getAllSubs(b2)) 
            Cs.Add(this.getAllSubs(b3))
        let newCs = Array.concat Cs
        multmod2.gemmBatchedMult (Array.concat As) (Array.concat Bs) newCs n
        this.setAllSubsByTask task newCs
            
     member this.chooseMultiplication(bound1, bound2, bound3) =
        let (b1, b2, b3) = Bounds(bound1), Bounds(bound2), Bounds(bound3)
        if b1.getLength >= 16
        then this.gemmMultiplicationByTask([|bound1, bound2, bound3|])
        else this.allSimpleMultiplication(b1, b2, b3)

     member this.chooseMultiplicationByTask(task : array<_>) =
        let (b1, _, _) = task.[0]
        let len = Bounds(b1).getLength
        if  len >= 16
        then this.gemmMultiplicationByTask(task)
        elif 2 >= len
        then this.allSimpleMultiplicationByTask(task, true)
        else this.allSimpleMultiplicationByTask(task, false)
     
     member this.finalCheck(N) = 
        Ts.Item(N).[fullSize - 1] > 0.0f
