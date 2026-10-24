namespace SnakeGame

open System



type StopToken() =
    member val StopReguested = false with get, set

type fitness = int


type ResultThusFar = { turns : int; score :int; }

/// data about what happened in a turn
type TurnResult = 
    | GameOver of ResultThusFar
    | AteFruit of ResultThusFar
    | TurnOK of ResultThusFar

    static member isOK r= 
        match r with
        | TurnOK(_) -> true
        |_-> false

    static member isGameOver r= 
        match r with
        | GameOver(_) -> true
        |_-> false

    static member unwrapOK r= 
        match r with
        | TurnOK(s) -> s
        |_->  raise <| new ArgumentException() 

    static member unwrapGameOver r= 
        match r with
        | GameOver(s)-> s
        |_->  raise <| new ArgumentException()    
    