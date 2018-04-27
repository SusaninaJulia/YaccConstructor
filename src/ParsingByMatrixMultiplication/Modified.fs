module Modified

open System
open GrammarBlock
open MatrixPerformanceBlock
open MatrixMultiplicationBlock


let main (str : int array) (gr : BooleanRulesHolder) (S : NonTerminal) = 
    
    let nts = gr.allNonterminals
    let N = nts.Length
    let n = str.Length + 1
    let TPs = TPMatrices(gr, n)

    let leftSubMatrix (l, m, l', m') = (l, (l + m) / 2, l', (l' + m') / 2)
    let rightSubMatrix (l, m, l', m') = ((l + m) / 2, m, (l' + m') / 2, m')
    let topSubMatrix (l, m, l', m') = (l, (l + m) / 2, (l' + m') / 2, m')
    let bottomSubMatrix (l, m, l', m') = ((l + m) / 2, m, l', (l' + m') / 2)

    let rightNeighbour (l, m, l', m') = (m, 2*m - l, l', m')
    let leftNeighbour (l, m, l', m') = (l, m, 2*l' - m', l')
    let leftGrounded (l, m, l', m') = (l, m, m, 2 * m - l)
    let rightGrounded (l, m, l', m') = (2 * l' - m', l', l', m')


    let constructLayer i = 
        let B = new ResizeArray<_>()
        let pow2i = 2.** float i
        for k in 0..(int (float(n) / pow2i) - 1) do
            let k2i = float k * pow2i
            if 2. * pow2i+ k2i < float (n + 1)
            then B.Add(k2i |> int, pow2i + k2i |> int, pow2i + k2i |> int, 2. * pow2i+ k2i|> int)
        B.ToArray()

    let rec completeLayer (M : array<_>) = 
        let (l, m, l', m') = M.[0]
        if m - l <> 1
        then
            let bottomSubLayer = Array.map (fun el  -> bottomSubMatrix el) M
            completeLayer(bottomSubLayer)
            completeVLayer(M)

    and completeVLayer (M : array<_>) = 

        let len = M.Length
        let (l, m, l', m') = M.[0]
        let leftSubLayer = Array.map (fun el -> leftSubMatrix el) M
        let rightSubLayer = Array.map (fun el -> rightSubMatrix el) M
        let topSubLayer = Array.map (fun el -> topSubMatrix el) M

        let multTask1 = Array.map (fun el -> (leftGrounded el, rightNeighbour el, el)) leftSubLayer
                        |> Array.append (Array.map (fun el -> (leftNeighbour el, rightGrounded el, el)) rightSubLayer)
        let multTask2 = Array.map (fun el -> (leftGrounded el, rightNeighbour el, el)) topSubLayer
        let multTask3 = Array.map (fun el -> (leftNeighbour el, rightGrounded el, el)) topSubLayer

        TPs.chooseMultiplicationByTask(multTask1)
        completeLayer (Array.append leftSubLayer rightSubLayer)
        TPs.chooseMultiplicationByTask(multTask2)
        TPs.chooseMultiplicationByTask(multTask3)
        completeLayer (topSubLayer)


    TPs.fill()
    TPs.fillDiagonal(str)
    let k = Math.Log(n + 1 |> float, 2.) |> int
    for i = 1 to k - 1 do
        completeVLayer (constructLayer i)
    TPs.finalCheck(S)