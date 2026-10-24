/// The kinds of brain a snake can have.
module SnakeEvo.Mind

open SnakeEvo.Game

type Mind =
    | Net of Brain.Brain
    /// The circuit is the fixed connectome, shared by every individual. Only the genome differs.
    | Wired of Connectome.Circuit * Connectome.Genome
    /// Compiled once per genome, not per move.
    | Grown of Neat.Net

/// What a mind computed, carrying what the matching view needs to draw it.
type Activity =
    /// Network weights and layer activations, input first.
    | Layers of Brain.Brain * float[][]
    /// Connectome circuit and the activity of each of its neurons.
    | Neurons of Connectome.Circuit * float[]
    /// NEAT net and the value of each of its nodes, by index.
    | Graph of Neat.Net * float[]

type Thought =
    { Input: float[]
      Activity: Activity
      Turn: Turn }

let think mind (input: float[]) =
    match mind with
    | Net brain ->
        let layers = Brain.think brain input

        { Input = input
          Activity = Layers(brain, layers)
          Turn = Brain.decide (Array.last layers) }
    | Wired(circuit, genome) ->
        let h = Connectome.think circuit genome input

        { Input = input
          Activity = Neurons(circuit, h)
          Turn = Connectome.decide circuit h }
    | Grown net ->
        let value = Neat.think net input

        { Input = input
          Activity = Graph(net, value)
          Turn = Brain.decide (Neat.outputs value) }
