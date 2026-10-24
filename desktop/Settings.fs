namespace SnakeGame

open System.Windows.Media




module Settings= 
    let rows = 10
    let columns = rows

    let mutable logging = false

    let BaseBrush = Brushes.WhiteSmoke //Brushes.LightGray
    let SnakeBrush = Brushes.Green
    let FruitBrush = Brushes.Red
    let BackGround = Brushes.WhiteSmoke


    let rng= new System.Random()
    ///how long one learning cycle takes
    let trainingGames = 100     
    
    let PopulationSize = 100             
    let Bias = 0.3
    /// how much intensity mutation has
    let MutationRate = 0.02            
    /// how often are weigths mutated
    let WeightMutationChance = 0.10  
    ///chance of crossover
    let CrossChance = 1.0
    /// structure of the neural network
    let NeuronLayerDim = [|24;18;3|]
    
    ///snake automatically dies after not eating for several turns to prevent running in circles indifinetely
    let TurnsUntilStarvingToDeath = rows * columns  / 2

    let relu = Array.map (fun value -> if value > 0.0 then value else 0.0)
    let softmax (values:float[]) = 
       //let max = Array.max values
       //let exps = Array.map (fun v -> exp(v-max)) values 
       //let sum =  Array.sum exps
       //Array.map (fun e -> e/sum) exps
       
       values
       //in this usecase softmax is identical to the id function
    let ActivationFunctions = [|relu;softmax|]
        
        
    