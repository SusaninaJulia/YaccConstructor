module MatrixMultiplicationBlock

open System
open Alea
open Alea.CUDA
open Alea.CUDA.IL
open Alea.CUDA.CULib
open Alea.CUDA.CULib.CUBLASInterop
open Alea.FSharp
open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.CudaBlas

[<AOTCompile>]
type MatrixMultiplyModule(target) =
    inherit GPUModule(target)

    [<Kernel;ReflectedDefinition>]
    member this.Kernel (A:deviceptr<float32>) (B:deviceptr<float32>) (C:deviceptr<float32>) fullSize blockSize a1 a2 a3 a4 a5 a6 =
        let bx = blockIdx.x
        let by = blockIdx.y
        let tx = threadIdx.x
        let ty = threadIdx.y

        let aBegin = fullSize * blockSize * by
        let aEnd = aBegin + fullSize - 1
        let aStep = blockSize
        let bStep = blockSize * fullSize

        let mutable a = fullSize * blockSize * by
        let mutable b = blockSize * bx
        while a <= aEnd do
            let c =  fullSize * blockSize * by + blockSize * bx
            if not (C.[c + fullSize * (ty + a6) + (tx + a5)]  > 0.0f)
            then 
                for k = 0 to blockSize - 1 do
                    if A.[a + fullSize * (ty + a2) + (k + a1)] > 0.0f && B.[b + fullSize * (k + a4) + (tx + a3)] > 0.0f
                    then C.[c + fullSize * (ty + a6) + (tx + a5)] <- 1.0f

            __syncthreads()

            a <- a + aStep
            b <- b + bStep


    member this.Mult(A:float32[], B:float32[],C:float32[], n, subSize, blockSize, a1, a2, a3, a4, a5, a6) =
        let worker = Worker.Default
        use dA = worker.Malloc(A)
        use dB = worker.Malloc(B)
        use dC = worker.Malloc(C)
        let grid = dim3(subSize/blockSize, subSize/blockSize)
        let block = dim3(blockSize, blockSize)
        let lp = LaunchParam(grid, block)
        this.GPULaunch <@ this.Kernel @> lp dA.Ptr dB.Ptr dC.Ptr n blockSize a1 a2 a3 a4 a5 a6
        dC.Gather()

type MatrixBatchedMultiplyModule(worker : Worker) =

    let blas = CUBLAS.Default
    let cublas = Alea.cuBLAS.Handle.Zero
    let trans = cublasOperation_t.CUBLAS_OP_N

    member this.gemmMult (A : float32[]) (B : float32[]) (C : float32[]) n =
        let blas = CUBLAS.Default
        let trans = cublasOperation_t.CUBLAS_OP_N
        use dalpha = worker.Malloc([|1.0f|])
        use dA = worker.Malloc(A)
        use dB = worker.Malloc(B)
        use dbeta = worker.Malloc([|1.0f|])
        use dC = worker.Malloc(C)
        blas.Sgemm(trans, trans, n, n, n, dalpha.Ptr, dB.Ptr, n, dA.Ptr, n, dbeta.Ptr, dC.Ptr, n)
        dC.Gather()

    member this.gemmBatchedMult (As : float32[][]) (Bs : float32[][]) (Cs : float32[][]) n =
        let batchCount = As.Length
        use dalpha = worker.Malloc([|1.0f|])
        use dbeta = worker.Malloc([|1.0f|])
        [for i in 0..batchCount-1 ->
            use dA = worker.Malloc(As.[i])
            use dB = worker.Malloc(Bs.[i])
            use dC = worker.Malloc(Cs.[i])
            blas.Sgemm(trans, trans, n, n, n, dalpha.Ptr, dB.Ptr, n, dA.Ptr, n, dbeta.Ptr, dC.Ptr, n)
            dC.Gather()] |> Array.ofList
    
    member this.gemmBatchedCublasMult(arrA : float32[]) (arrB : float32[]) (arrC : float32[]) n batchCount =
        let trans = Alea.cuBLAS.Operation.N
        use dalpha = worker.Malloc([|0.0f|])
        use dbeta = worker.Malloc([|0.0f|])
        use dA = worker.Malloc(arrA)
        use dB = worker.Malloc(arrB)
        use dC = worker.Malloc(arrC)
        //status = not_initialized ¯\_(ツ)_/¯
        let res = Alea.cuBLAS.Interop.cublasSgemmBatched(cublas, trans, trans, n, n, n, NativeInterop.NativePtr.ofNativeInt<float32> dalpha.Ptr.Handle, dA.Ptr.Handle,n, dB.Ptr.Handle, n, NativeInterop.NativePtr.ofNativeInt<float32> dbeta.Ptr.Handle, dC.Ptr.Handle, n, batchCount)
        printfn "%s" (res.ToString())
        dC.Gather()
 
type MatrixManagedCudaModule() = 
    
    let trans = Operation.NonTranspose
    let alf = 1.0f
    let bet = 0.0f

    member this.gemmMult (A : float32[]) (B : float32[]) (C : float32[]) (n : int) =
        let handle = new CudaBlas()
        let dA  = new CudaDeviceVariable<float32>(new SizeT(n * n))
        let dB  = new CudaDeviceVariable<float32>(new SizeT(n * n))
        let dC  = new CudaDeviceVariable<float32>(new SizeT(n * n))
        dA.CopyToDevice(A)
        dB.CopyToDevice(B)
        dC.CopyToDevice(C)
        handle.Gemm(trans, trans, n, n, n, alf, dA, n, dB, n, bet, dC, n)
        dC.CopyToHost(C)
        dA.Dispose()
        dB.Dispose()
        dC.Dispose()
     
    member this.gemmBatchedMult  (A : float32[][]) (B : float32[][]) (C : float32[][]) (n : int) = 
        
        let handle = new CudaBlas()
        let batchCount = A.Length
        let toHost (data_h : float32[][]) =
            let size = new SizeT(n * n)
            let arrayOfarray_d = Array.init batchCount (fun _ -> new CudaDeviceVariable<float32>(size))
            Array.iteri (fun i (el : CudaDeviceVariable<float32>) -> el.CopyToDevice(data_h.[i])) arrayOfarray_d
            let data_d = new CudaDeviceVariable<CUdeviceptr>(new SizeT(batchCount))
            let ptrsToData_h = Array.map (fun (el : CudaDeviceVariable<_>) -> el.DevicePointer) arrayOfarray_d
            data_d.CopyToDevice(ptrsToData_h)
            data_d,  arrayOfarray_d, ptrsToData_h

        let free (cdv : CudaDeviceVariable<_>) (cdvArr : CudaDeviceVariable<_>[]) = 
            Array.iter (fun (el : CudaDeviceVariable<_>) -> el.Dispose()) cdvArr
            cdv.Dispose()

        let dA, arrOfarrsA, _ = toHost A 
        let dB, arrOfarrsB, _ = toHost B
        let dC, arrOfarrsC, fromHostC = toHost C   


        handle.GemmBatched(trans, trans, n, n, n, alf, dA, n, dB, n, bet, dC, n, batchCount)
        free dA arrOfarrsA
        free dB arrOfarrsB
        Array.iteri (fun i (el : CudaDeviceVariable<float32>) -> el.CopyToHost(C.[i])) arrOfarrsC
        dC.CopyToHost(fromHostC)
        free dC arrOfarrsC