namespace SnakeGame
open System.Threading.Tasks
open System

module Misc =
    let inline flip f x y =  f y x
       
    /// in place mapping 
    let inline mapInPlace f (arr: 'a []) = 
        for i in [0..arr.Length-1] do
            arr.[i] <- f arr.[i]  
        arr

    /// like normal fold2 but runs an extra folding iteration on bias
    let fold2Biased bias folder (state: float) (array1:float[]) (array2:float []) =
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(folder)
        let mutable state = state 
        for i = 0 to array1.Length-1 do 
            state <- f.Invoke(state,array1.[i],array2.[i])
        f.Invoke(state,bias,array2.[array1.Length])

    /// mapInPlace indexed
    let inline mapInPlace_i f (arr: 'a []) = 
        for i in [0..arr.Length-1] do
            arr.[i] <- f i arr.[i]  
        arr

    /// map2 indexed
    let inline map2i (f:int->'a->'b->'c) (arr1:'a[]) (arr2:'b[])=
        [|
            for i in 0..arr1.Length-1 -> 
                f i (arr1.[i]) (arr2.[i])
        |]

    /// returning sort-in-place
    let inline sortInPlaceBy (selector: 'a -> 'key) (arr: 'a []) =  
        do Array.sortInPlaceBy selector arr
        arr

    let log (str:string) =
        Task.Run (fun _ -> Diagnostics.Debug.WriteLine str ) |> ignore 


    let inline DoNothing _ _ _  = ()

    type System.Random with
    member this.getSign ()= 
        this.Next () > this.Next ()

