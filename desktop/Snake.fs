namespace SnakeGame

open System.Collections.Generic

/// direcions to which the snake can move
type Directions = 
    | Left = 0
    | Up = 1
    | Right = 2
    | Down = 3
    
    
/// indicates whether a cell is empty, fruit or snake 
[<StructuralComparison;StructuralEquality>]
type CellState = Empty | Fruit | Snake


/// holds data about the snake
[<Struct>]
type Snake =  
    {
        body : LinkedList<struct (int*int)>   // body needs to be ordered because of movement => head moves forward and tails gets removed
        direction : Directions
        score : int
        turnsWithoutSnack : int
        turns : int
    }
    member this.Last = this.body.Last
    member this.Head = this.body.First
    static member create x y dir  =
        let b = new LinkedList<struct(int*int)>()
        do b.AddFirst(struct(x,y)) |> ignore
        {
            body = b   
            direction = dir
            score = 0;
            turnsWithoutSnack = 0;
            turns = 0
        }