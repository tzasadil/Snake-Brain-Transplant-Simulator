/// Brains wired by a real connectome: the fruit fly steering circuit (web/tools/extract_fly.py)
/// and the whole C. elegans worm (web/tools/extract_worm.py). Wiring, signs, and synapse strengths are fixed.
/// Evolution tunes only how the snake's senses feed the input neurons, plus a gain and threshold per neuron,
/// like ChessFly (huggingface.co/mlabonne/chessfly). Thresholds alone cannot amplify a weak path, hence the gains.
module SnakeEvo.Connectome

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
      /// Soma position in the species' drawing view, 0 to 1.
      X: float
      Y: float }

type Circuit =
    { Neurons: Neuron[]
      /// Sensory neurons that receive the snake's senses.
      Inputs: int[]
      /// Steering readout: neuron and weight per side.
      Left: (int * float)[]
      Right: (int * float)[]
      /// Edges grouped by postsynaptic neuron j: Pre and Weight from Start[j] to Start[j + 1] - 1.
      /// One pass over this computes every neuron's input.
      Start: int[]
      Pre: int[]
      Weight: float[]
      /// Gap junctions grouped the same way: neuron j receives GapWeight times GapPeer's activity.
      /// Electrical synapses conduct both ways and are common in the worm. The fly file has none.
      GapStart: int[]
      GapPeer: int[]
      GapWeight: float[]
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

/// A circuit file as parsed, before validation.
type Raw =
    { /// Type, side, role, x, y.
      Neurons: (string * string * string * float * float)[]
      Inputs: int[]
      Left: (int * float)[]
      Right: (int * float)[]
      /// Flat pre, post, weight triples.
      Edges: float[]
      /// Flat from, to, weight triples, one per direction.
      Gaps: float[]
      Cloud: float[]
      Steps: int }

/// Groups triples by their second index, the neuron they act on, so one pass computes every neuron's input.
let private group n (triples: (int * int * float)[]) =
    let sorted = triples |> Array.sortBy (fun (_, post, _) -> post)
    let start = Array.zeroCreate<int> (n + 1)

    for _, post, _ in sorted do
        start[post + 1] <- start[post + 1] + 1

    for j in 1..n do
        start[j] <- start[j] + start[j - 1]

    start, sorted |> Array.map (fun (pre, _, _) -> pre), sorted |> Array.map (fun (_, _, w) -> w)

/// Validates the extracted circuit. It is the boundary between the JSON file and the simulation.
let build (raw: Raw) =
    let neurons, inputs, left, right, edges, gaps, cloud, steps =
        raw.Neurons, raw.Inputs, raw.Left, raw.Right, raw.Edges, raw.Gaps, raw.Cloud, raw.Steps

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
    elif edges.Length % 3 <> 0 || gaps.Length % 3 <> 0 then
        Error "edges and gaps must be pre, post, weight triples"
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
        let triples (flat: float[]) =
            Array.init (flat.Length / 3) (fun k -> int flat[3 * k], int flat[3 * k + 1], flat[3 * k + 2])

        let chemical, electrical = triples edges, triples gaps

        if not (Array.append chemical electrical |> Array.forall (fun (a, b, _) -> valid a && valid b)) then
            Error "edge points outside the circuit"
        else
            let start, pre, weight = group n chemical
            let gapStart, gapPeer, gapWeight = group n electrical

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
                  Pre = pre
                  Weight = weight
                  GapStart = gapStart
                  GapPeer = gapPeer
                  GapWeight = gapWeight
                  Cloud = cloud
                  Steps = steps }

/// Activity of every neuron after `c.Steps` updates of h <- relu(gain * (W h + G h + drive) - threshold).
/// Gap junctions add a share of the partner's activity. A difference term, G (h_peer - h), would need activity
/// carried over between steps. Without it, it acts as self-inhibition and makes the result flip with odd and even steps.
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

            for e in at j c.GapStart .. at (j + 1) c.GapStart - 1 do
                sum <- sum + at e c.GapWeight * at (at e c.GapPeer) h

            next[j] <- max 0.0 (at j gain * sum - at j g.Threshold)

        h <- next

    h

/// Turn toward the side whose steering neurons fire more: DNa02 in walking flies (Rayshubskiy et al., eLife 2025),
/// dorsal against ventral head motor neurons in the worm. A small margin keeps near-ties going straight.
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

/// The network's 561 genes at a 10% rate mutate about 56 per child. Circuits have 1,300 to 1,600 genes,
/// so the rate scales down to keep the same number of mutations per child.
let private mutationsPerChild = 56.0

let crossover (rng: Source) (a: Genome) (b: Genome) =
    let genes = a.Encoder.Length * a.Encoder[0].Length + a.Gain.Length + a.Threshold.Length
    let gene = Brain.inheritGene rng (mutationsPerChild / float genes)

    { Encoder = Array.map2 (Array.map2 gene) a.Encoder b.Encoder
      Gain = Array.map2 gene a.Gain b.Gain
      Threshold = Array.map2 gene a.Threshold b.Threshold }
