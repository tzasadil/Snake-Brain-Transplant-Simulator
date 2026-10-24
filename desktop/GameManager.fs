namespace SnakeGame

open System.Collections.Generic
open System

/// class responsible for the core game loop
type GameManager(draw:(CellState -> int -> int -> unit)) =

    let mutable seed = Settings.rng.Next ()
    let  rng = new Random(seed)
    
    
    //without this queue, user input would sometimes get ignored
    let directionChange = new Queue<Directions>()  
    

    let randomDirection () = rng.Next 4 |> enum<Directions>
    
    /// holds 'what is where' information 
    let stateField = Array2D.init Settings.columns  Settings.rows <| fun x y -> 
        do draw CellState.Empty x y
        CellState.Empty

    
    let changeCell st x y = 
        do draw st x y
        do Array2D.set stateField x y st

    let rec GenerateFruit () =    
            let x = rng.Next(0, Settings.columns)
            let y = rng.Next(0, Settings.rows)

            if stateField.[x,y] = CellState.Empty
            then struct (x,y)
            else GenerateFruit ()  //repeat until an empty cell is found

    /// value tuple equivalent of <|| 
    let (<~|) f (struct (x,y)) = f x y     
    
    let addTuples struct (a,b) struct (c,d) = struct (a+c,b+d)

    // start in the middle
    let startX = (Settings.columns-1) / 2
    let startY = (Settings.rows-1) / 2

    
    let mutable snake = Snake.create startX startY <| randomDirection()
    do changeCell CellState.Snake startX startY
    
    let mutable fruit = GenerateFruit ()
    do changeCell CellState.Fruit <~| fruit
    
    let outOfBounds x y = x >= Settings.columns || x < 0 || y < 0 || y >= Settings.rows
    
    let CurrentScore () = {score=snake.score ;turns = snake.turns}   
    

    ///restarts the game so it can be reused instead of allocated anew
    member _.Restart ()= 
        Seq.iter ((<~|) (changeCell CellState.Empty)) snake.body 
        changeCell CellState.Empty <~| fruit     
        
        snake <- Snake.create startX startY <| randomDirection()
        changeCell CellState.Snake startX startY
        fruit <- GenerateFruit ()
        changeCell CellState.Fruit <~| fruit 

    ///clear possible visual bugs and restart
    member self.HardRestart ()=
         Array2D.iteri (fun x y _ -> changeCell CellState.Empty x y) stateField
         self.Restart ()

    /// move relative to the screen
    /// used for player as it is more natural to use
    member _.MoveAbs dir = 
        let dirToCoords = 
            match dir with
                | Directions.Left -> struct (-1,0)
                | Directions.Up -> struct (0,-1)
                | Directions.Right -> struct (1,0)
                | Directions.Down -> struct (0,1)
                | _ -> invalidOp "unknown case"
        let struct (newX,newY) = addTuples snake.Head.Value dirToCoords 

        //move when the cell is empty
        let moveToEmpty ()= 
            do changeCell CellState.Empty <~| snake.Last.Value
            do changeCell CellState.Snake newX newY 
            snake.body.RemoveLast () |> ignore
            do snake.body.AddFirst(struct (newX,newY)) |> ignore
            snake <- {snake with 
                        turns = snake.turns + 1;
                        direction = dir; 
                        turnsWithoutSnack = snake.turnsWithoutSnack + 1}
            TurnOK <| CurrentScore ()

        if outOfBounds newX newY
        then  GameOver <| CurrentScore () 
        else match stateField.[newX, newY] with 
                | CellState.Fruit ->
                    do changeCell CellState.Snake newX newY 
                    do snake.body.AddFirst(struct (newX,newY)) |> ignore
                    do fruit <- GenerateFruit ()
                    do changeCell CellState.Fruit <~| fruit
                    snake <- {snake with 
                                direction = dir; 
                                turnsWithoutSnack = 0;
                                turns = snake.turns + 1;
                                score = snake.score + 1}
                    AteFruit <| CurrentScore () 
                | CellState.Empty -> moveToEmpty ()
                | CellState.Snake when snake.Last.Value = (newX,newY) -> moveToEmpty () //the tail moves, so the snake does not collide with itself
                | CellState.Snake -> GameOver <| CurrentScore () 

    /// move relative to where the snake is facing 
    /// prefered by AI because it's easier to train (doesn't need to figure out which direction is which)
    member this.MoveRel dir =    
        if snake.turnsWithoutSnack = Settings.TurnsUntilStarvingToDeath  + snake.score * 2   // add score to make lategame easier
        then GameOver {score=snake.score  ;turns = snake.turns * 3/2}
        else
            let absDirection =
                let diff = 
                    match dir with  // snake can choose to go right, left or forward
                    | Directions.Right -> 1 
                    | Directions.Left -> -1
                    |_ -> 0
                (diff + 4 + int snake.direction ) % 4 |> enum<Directions> // the +4 inside is there if diff = -1 to make the expression a positive integer
            this.MoveAbs absDirection

    /// provides the AI all the necessary information about its surroundings
    member _.LookAround ()=
            let tail = snake.Last.Value
        
            /// direction vectors ordered clockwise from Direction.Left
            let dirs = [|struct(- 1,0);struct(- 1,-1);struct(0,-1); struct(1,-1); struct(1,0);struct(1,1);struct(0,1);struct(-1,1)|] 
        
            [| for i in [0..7] do
                // rotate visual inforamation relative to where the snake is looking
                // *2 is there because snake cannot move in diagonal directions
                let dir = dirs.[(i + int snake.direction * 2)%8] 

        
                let mutable distToWall = None
                let mutable distToSnake = None
                let mutable distToFruit = None

                /// seeks the nearest snakecell, fruit and wall in a direction
                let rec lookInDirection cell dist = 
                    let newCell = addTuples cell dir

                    if outOfBounds <~| newCell
                    then distToWall <- Some dist  //terminates recursion
                    else
                        match Array2D.get stateField <~| newCell with 
                            | CellState.Snake when tail <> newCell-> if distToSnake.IsNone then distToSnake <- Some dist 
                            | CellState.Fruit-> if distToFruit.IsNone then distToFruit <- Some dist
                            | _ -> ()
                        lookInDirection newCell (dist+1)
     
                do lookInDirection snake.Head.Value 1

                // scale input activation based on distance
                let toVal = Option.map (fun v->1.0/float v |> float ) >> Option.defaultValue 0.0   
                yield toVal distToWall
                yield toVal distToSnake
                yield toVal distToFruit 
            |]
        

    member _.GetSeed ()= seed

    ///move the snake; automatically called by the timer
    member this.Move () = 
        if directionChange.Count > 0 
        then 
            let dir = directionChange.Dequeue()

            // movement in opposite direction allowed only if snake has lenght 1
            if (int dir + 2) % 4 <> int snake.direction || snake.body.Count = 1 
                then this.MoveAbs (dir) 
                else this.Move ()
        else this.MoveRel Directions.Up

    /// schedule the snake to change direction on the next update
    member _.ChangeDirection dir = directionChange.Enqueue(dir)