/// The kinds of brain a snake can have.
module SnakeEvo.Mind

open SnakeEvo.Game

type Mind =
    | Net of Brain.Brain
    /// The circuit is the fixed connectome, shared by every individual. Only the genome differs.
    | Wired of Connectome.Circuit * Connectome.Genome
    /// Compiled once per genome, not per move.
    | Grown of Neat.Net
    | Rules of Program.Tree
    | Hamiltonian of Hamilton.Cycle
    /// A frozen Q-table. It plays greedily and no longer learns.
    | Tabular of QTable.Table

/// What a mind computed, carrying what the matching view needs to draw it.
type Activity =
    /// Network weights and layer activations, input first.
    | Layers of Brain.Brain * float[][]
    /// Connectome circuit and the activity of each of its neurons.
    | Neurons of Connectome.Circuit * float[]
    /// NEAT net and the value of each of its nodes, by index.
    | Graph of Neat.Net * float[]
    /// Program tree and the answer to each test on the way to the chosen move.
    | Path of Program.Tree * bool list
    /// Cycle and the positions behind the move.
    | Route of Hamilton.Cycle * Hamilton.Plan
    /// Q-table, the game it looked at, and the current state.
    | Policy of QTable.Table * Game * state: int

type Thought =
    { Input: float[]
      Activity: Activity
      Turn: Turn }

/// Takes the whole game, because the Hamiltonian cycle needs positions, not only senses.
/// Input is the senses either way, so the board can draw the rays.
let think mind (g: Game) =
    let input = vision g

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
    | Rules tree ->
        let turn, path = Program.decide tree input

        { Input = input
          Activity = Path(tree, path)
          Turn = turn }
    | Hamiltonian cycle ->
        let turn, plan = Hamilton.decide cycle g

        { Input = input
          Activity = Route(cycle, plan)
          Turn = turn }
    | Tabular table ->
        let s = QTable.state g

        { Input = input
          Activity = Policy(table, g, s)
          Turn = QTable.greedy table s }
