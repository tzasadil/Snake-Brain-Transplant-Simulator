/// The neural network and its genetics, ported from AI.fs.
module SnakeEvo.Brain

open SnakeEvo.Native
open SnakeEvo.Rng
open SnakeEvo.Game

/// layer -> neuron -> incoming weights. The last weight of each neuron multiplies the bias input.
type Brain = float[][][]

/// Vision inputs, 18 hidden neurons, 3 outputs: turn left, go straight, turn right.
let layout = [| inputCount; 18; 3 |]

let private biasInput = 0.3
let private mutationChance = 0.10
let private mutationSigma = 0.2

let random (rng: Source) : Brain =
    layout
    |> Array.pairwise
    |> Array.map (fun (inputs, neurons) ->
        Array.init neurons (fun _ -> Array.init (inputs + 1) (fun _ -> rng.Float() * 2.0 - 1.0)))

/// Activations of every layer, input first, so the visualizer can light each neuron.
/// Hidden layers use ReLU. The output layer is linear, because argmax ignores softmax anyway.
let think (brain: Brain) (input: float[]) =
    let layers = Array.zeroCreate<float[]> (brain.Length + 1)
    layers[0] <- input

    for l in 0 .. brain.Length - 1 do
        let prev = layers[l]

        layers[l + 1] <-
            brain[l]
            |> Array.map (fun w ->
                let mutable sum = 0.0

                for i in 0 .. prev.Length - 1 do
                    sum <- sum + at i prev * at i w

                sum <- sum + biasInput * at prev.Length w
                if l < brain.Length - 1 then max sum 0.0 else sum)

    layers

/// Strongest output wins. Ties go to the first, like Seq.maxBy in the original.
let decide (output: float[]) =
    let mutable best = 0

    for i in 1 .. output.Length - 1 do
        if output[i] > output[best] then
            best <- i

    [| TurnLeft; Straight; TurnRight |][best]

/// Takes each weight from a random parent, then mutates 10% of weights by Gaussian noise (sigma 0.2).
/// The original nudged weights by a fixed 0.02, and only those taken from the second parent
/// (operator precedence in AI.fs). Against initial weights in (-1, 1) that step barely moved anything,
/// so evolution only reshuffled the random start and could not learn to use new inputs.
let crossover (rng: Source) (a: Brain) (b: Brain) : Brain =
    let pick w1 w2 =
        let w = if rng.Float() > 0.5 then w1 else w2

        if rng.Float() > mutationChance then
            w
        else
            // Box-Muller transform. The max keeps log away from 0.
            let u = max (rng.Float()) 1e-12
            w + mutationSigma * sqrt (-2.0 * log u) * cos (2.0 * System.Math.PI * rng.Float())

    Array.map2 (Array.map2 (Array.map2 pick)) a b
