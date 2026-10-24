namespace SnakeGame

open System

type NN = float [] [] []

/// where the neural network comes from
type NN_source =
    | Net of NN
    | Clone of NN
    | Path of string
    | GenerateRandom


/// the Neural network
type AI (brainSource : NN_source) = 
    let rng = Settings.rng 

    let generateBrain ()= // layer => neuron => weight
        let a = rng.NextDouble ()
        Settings.NeuronLayerDim 
            |> Array.pairwise
            |> Array.map (fun (last,next) ->
                Array.init next (fun _ ->    
                    Array.init (last+1) (fun _ -> (rng.NextDouble () * 2.0 - 1.0)))) // weigths randomly between (-1,1)          // the last+1 is because of bias   
       
    /// load the AI from a file
    let loadBrain (filePath:string) =
        let stream = new System.IO.StreamReader(filePath)
        let ser = new Newtonsoft.Json.JsonSerializer()
        let jsr= new Newtonsoft.Json.JsonTextReader(stream)
        ser.Deserialize<NN>(jsr)
        

    
    let brain = 
        match brainSource with 
        | Net(nn) -> nn
        | Clone(nn) -> (Array.map >> Array.map >> Array.map) id nn
        | Path(str) -> loadBrain str
        | GenerateRandom -> generateBrain ()
    
    ///runs an input through the neural network and returns a vector of direction probabilities 
    let ForwardPropagation inp =
        Array.fold2 (fun  state activation layer-> 
           Array.map (fun  nextNeuron -> //all weights that go to a neuron in next layer
                Misc.fold2Biased Settings.Bias (fun acc x weight-> acc + x * weight) 0.0 state nextNeuron) layer 
                |> activation
           ) inp Settings.ActivationFunctions brain
    
    // big intensity in the first layer, small in the last layer
    static let CalcIntensity i = (Settings.NeuronLayerDim.Length - i)/2 |> float 

    ///weight mutation
    static let mutateWeight (rng:Random) mutationIntensity weight = 
        if rng.NextDouble () > Settings.WeightMutationChance
        then weight
        else  
            rng.Next () - rng.Next () 
            |> sign 
            |> float 
            |> (*) Settings.MutationRate 
            //|> (*) mutationIntensity
            |> (+) weight


    member _.Brain = brain
    member _.Rng = rng

    /// save the AI to a file
    member _.saveBrain (filePath:string) =
        let s = Newtonsoft.Json.JsonConvert.SerializeObject brain 
        IO.File.WriteAllText(filePath,s)
             

    ///lets the AI decide a direction it wants to go to
    member _.TakeTurn (game: GameManager) =
            game.LookAround () 
            |> ForwardPropagation
            |> Seq.indexed
            |> Seq.maxBy snd
            |> (fst >> enum<Directions>)
            |> game.MoveRel

    /// Randomly combines two specimen into a new one
    static member CrossOver (ai_1:AI) (ai_2:AI)= 
        let pickAndMutateWeight i weight1 weight2 =
            if ai_1.Rng.NextDouble() > 0.5
            then weight1
            else weight2
                |> mutateWeight ai_1.Rng (CalcIntensity i)
        if ai_1.Rng.NextDouble() > Settings.CrossChance   
        then ai_1
        else
            (ai_1.Brain, ai_2.Brain)
            ||> Misc.map2i (fun i->Array.map2(Array.map2( pickAndMutateWeight i)))
            |> Net
            |> AI