/// Fruit fly mode: a steering subcircuit of the male fly connectome drives the snake.
/// Built by web/tools/extract_fly.py. Wiring, signs, and synapse strengths are fixed by the connectome.
/// Evolution tunes only how the snake's senses feed the input neurons, plus a gain and threshold per neuron,
/// like ChessFly (huggingface.co/mlabonne/chessfly). Thresholds alone cannot amplify a weak path, hence the gains.
module SnakeEvo.Fly

open SnakeEvo.Native
open SnakeEvo.Rng
open SnakeEvo.Game

type Role =
    | Input
    | Inter
    | Output

type Neuron =
    { Type: string
      Side: string
      Role: Role
      /// Soma position in a front view, 0 to 1.
      X: float
      Y: float }

type Circuit =
    { Neurons: Neuron[]
      /// Visual projection neurons that receive the snake's senses.
      Inputs: int[]
      /// Steering readout: neuron and weight per side.
      Left: (int * float)[]
      Right: (int * float)[]
      /// Edges grouped by postsynaptic neuron j: Pre and Weight from Start[j] to Start[j + 1] - 1.
      /// One pass over this computes every neuron's input.
      Start: int[]
      Pre: int[]
      Weight: float[]
      /// Soma positions sampled from the whole brain, flat x, y pairs. Only drawn, never simulated.
      Cloud: float[]
      /// Update steps per snake move: the longest input-to-steering path the extraction kept.
      /// Activity resets each move, so the same position always gives the same turn and replays stay exact.
      Steps: int }

type Genome =
    { /// Snake input i -> input neuron m.
      Encoder: float[][]
      /// Natural log of each neuron's gain, so gains stay positive.
      Gain: float[]
      Threshold: float[] }

/// Validates the extracted circuit. It is the boundary between the JSON file and the simulation.
let build (neurons: (string * string * string * float * float)[]) (inputs: int[]) (left: (int * float)[]) (right: (int * float)[]) (edges: float[]) (cloud: float[]) (steps: int) =
    let n = neurons.Length
    let valid i = i >= 0 && i < n

    let roles =
        neurons
        |> Array.map (fun (_, _, role, _, _) ->
            match role with
            | "input" -> Ok Input
            | "inter" -> Ok Inter
            | "output" -> Ok Output
            | other -> Error $"unknown role {other}")

    if n = 0 then
        Error "no neurons"
    elif roles |> Array.exists Result.isError then
        Error "unknown neuron role"
    elif edges.Length % 3 <> 0 then
        Error "edges must be pre, post, weight triples"
    elif cloud.Length % 2 <> 0 then
        Error "cloud must be x, y pairs"
    // More steps cost time on every move, and paths longer than 10 hops carry almost nothing after normalization.
    elif steps < 1 || steps > 10 then
        Error "steps must be 1 to 10"
    elif not (Array.forall valid inputs) || Array.isEmpty inputs then
        Error "bad input neurons"
    elif not (Array.forall (fst >> valid) (Array.append left right)) || Array.isEmpty left || Array.isEmpty right then
        Error "bad steering neurons"
    else
        let triples = Array.init (edges.Length / 3) (fun k -> int edges[3 * k], int edges[3 * k + 1], edges[3 * k + 2])

        if not (triples |> Array.forall (fun (pre, post, _) -> valid pre && valid post)) then
            Error "edge points outside the circuit"
        else
            let sorted = triples |> Array.sortBy (fun (_, post, _) -> post)
            let start = Array.zeroCreate<int> (n + 1)

            for _, post, _ in sorted do
                start[post + 1] <- start[post + 1] + 1

            for j in 1..n do
                start[j] <- start[j] + start[j - 1]

            Ok
                { Neurons =
                    Array.map2
                        (fun (t, side, _, x, y) role ->
                            { Type = t
                              Side = side
                              Role = Result.defaultValue Inter role
                              X = x
                              Y = y })
                        neurons
                        roles
                  Inputs = inputs
                  Left = left
                  Right = right
                  Start = start
                  Pre = sorted |> Array.map (fun (pre, _, _) -> pre)
                  Weight = sorted |> Array.map (fun (_, _, w) -> w)
                  Cloud = cloud
                  Steps = steps }

/// Activity of every neuron after `c.Steps` updates of h <- relu(gain * (W h + drive) - threshold).
let think (c: Circuit) (g: Genome) (input: float[]) =
    let n = c.Neurons.Length
    let drive = Array.zeroCreate<float> n

    for m in 0 .. c.Inputs.Length - 1 do
        let mutable sum = 0.0

        for i in 0 .. input.Length - 1 do
            sum <- sum + at i input * at m (at i g.Encoder)

        drive[at m c.Inputs] <- sum

    let gain = g.Gain |> Array.map exp
    let mutable h = Array.zeroCreate<float> n

    for _ in 1 .. c.Steps do
        let next = Array.zeroCreate<float> n

        for j in 0 .. n - 1 do
            let mutable sum = at j drive

            for e in at j c.Start .. at (j + 1) c.Start - 1 do
                sum <- sum + at e c.Weight * at (at e c.Pre) h

            next[j] <- max 0.0 (at j gain * sum - at j g.Threshold)

        h <- next

    h

/// Turn toward the side whose steering neurons fire more, like DNa02 in walking flies
/// (Rayshubskiy et al., eLife 2025). A small margin keeps near-ties going straight.
let decide (c: Circuit) (h: float[]) =
    let side (neurons: (int * float)[]) = neurons |> Array.sumBy (fun (i, w) -> w * h[i])
    let left, right = side c.Left, side c.Right
    let margin = 0.1 * (left + right) + 1e-9

    if left - right > margin then TurnLeft
    elif right - left > margin then TurnRight
    else Straight

let random (c: Circuit) (rng: Source) =
    let n = c.Neurons.Length

    { Encoder = Array.init inputCount (fun _ -> Array.init c.Inputs.Length (fun _ -> rng.Float() * 2.0 - 1.0))
      Gain = Array.init n (fun _ -> rng.Float() - 0.5)
      Threshold = Array.init n (fun _ -> (rng.Float() - 0.5) * 0.2) }

/// About 1,250 genes against the network's 561, so a lower rate keeps about 56 mutations per child, as there.
let private mutationChance = 0.045

let crossover (rng: Source) (a: Genome) (b: Genome) =
    let gene = Brain.inheritGene rng mutationChance

    { Encoder = Array.map2 (Array.map2 gene) a.Encoder b.Encoder
      Gain = Array.map2 gene a.Gain b.Gain
      Threshold = Array.map2 gene a.Threshold b.Threshold }
