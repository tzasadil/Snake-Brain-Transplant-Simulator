namespace SnakeGame

open System

type Population ={
    generation: int
    snakes: AI[]
}
type score = int
type traingResult= (score * Snake) []

/// population management
module Population = 
    let rng = new Random(Settings.rng.Next())

    /// load a population from a directory
    let Load directory = 
        if IO.Directory.Exists directory
            then IO.Directory.EnumerateFiles directory |> Seq.map (Path >> AI) |> Array.ofSeq
            else  failwith "directory not found"

    /// save the population to a directory
    let Save (snakes:seq<AI>) directory  = 
        snakes
        |> Seq.indexed
        |> Seq.iter (fun (i,ai) ->  
            do ai.saveBrain <| sprintf @"%s\%d" directory i)

    let GenerateRandomly () = Array.init Settings.PopulationSize (fun _ -> AI GenerateRandom)
      
    let CalculateFitness (res:ResultThusFar) : fitness =
        res.score//*(Settings.rows + Settings.columns)*3/2 - res.turns

    ///creates the next generation from the results of last generation
    let GetNextGeneration (pop:('a*'b*AI) []) = 
        let third (_,_,x)= x
        let RNG = (pop.[0]|>third).Rng
        let len = pop.Length
        let getRand () = RNG.Next len |> RNG.Next

        Array.init len (fun i -> 
            if i < 2
            then pop.[i]|>third
            else AI.CrossOver (pop.[getRand()] |> third) (pop.[getRand()] |> third) 
         )     
    /// play the game forever
    let play (game :GameManager) (ai:AI) = 
        do game.Restart ()
        Seq.initInfinite (fun _ -> ai.TakeTurn game)
    
    ///makes the AI play until it loses
    let playUntilLose (game :GameManager)  (ai:AI) = 
        play game ai 
        |> Seq.find TurnResult.isGameOver
        |> TurnResult.unwrapGameOver
        |> fun res -> CalculateFitness res, res, ai
       
    /// trains a population for n generations; can be stopped by the stopToken
    let Train n snakes (stopToken:StopToken) log= 
        let gamePool = Array.init Settings.PopulationSize <| fun _ -> GameManager(Misc.DoNothing)
        Seq.init n id 
        |> Seq.fold (
            fun population i -> 
                if stopToken.StopReguested 
                then population 
                else 
                    let res = Array.zip gamePool population 
                              |> Array.Parallel.map (fun (a,b) -> playUntilLose a b) 
                              |> Array.sortByDescending (fun (fit, _, _) -> fit)
                        
                    let (fit,{ turns = turns; score = score },best) = Array.head res
                    
                    
                    let New = res |> GetNextGeneration 
                     
                    do log best i score
                    New
            ) snakes