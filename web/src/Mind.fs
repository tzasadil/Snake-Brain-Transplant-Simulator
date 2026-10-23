/// The two kinds of brain a snake can have.
module SnakeEvo.Mind

open SnakeEvo.Game

type Mind =
    | Net of Brain.Brain
    /// The circuit is the fixed connectome, shared by every fly. Only the genome differs.
    | Fly of Fly.Circuit * Fly.Genome

/// What a mind computed, carrying what the matching view needs to draw it.
type Activity =
    /// Network weights and layer activations, input first.
    | Layers of Brain.Brain * float[][]
    /// Fly circuit and the activity of each of its neurons.
    | Neurons of Fly.Circuit * float[]

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
    | Fly(circuit, genome) ->
        let h = Fly.think circuit genome input

        { Input = input
          Activity = Neurons(circuit, h)
          Turn = Fly.decide circuit h }
