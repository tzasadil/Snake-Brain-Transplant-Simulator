/// Spiking neurons whose synapses learn during play from a dopamine-like reward signal.
/// The learning rule is REINFORCE (Williams 1992) over spike counts, with eligibility traces and a reward
/// baseline. Spike timing plays no part, so it is not the spike-timing plasticity of Izhikevich (2007).
/// The senses drive input neurons, which drive leaky integrate-and-fire hidden neurons over fixed random wiring.
/// Inputs and hidden neurons both feed the 3 output neurons, and only those output synapses learn.
/// The hidden layer forms combinations of senses. A control without it learned much more slowly, though it
/// caught up by game 300 (docs/experiment-reports.md).
module SnakeEvo.Spiking

open SnakeEvo.Native
open SnakeEvo.Rng
open SnakeEvo.Game

/// Senses that can be negative: the fruit compass, when the fruit is behind or to the left. Each gets a second
/// input neuron for its negative part. Every other sense is 0 or more, so a second neuron would never fire.
let private negative = [| 24; 25 |]

/// One input neuron per sense, then one per negative part.
let inputs = inputCount + negative.Length
let hidden = 40
/// Neurons that feed the outputs: every input, then every hidden neuron.
let sources = inputs + hidden
let outputs = 3
let neurons = sources + outputs
/// Simulation steps per move. Activity resets each move, so the same position always gives the same move.
let steps = 20

/// Share of membrane potential kept from one step to the next.
let private leak = 0.8
let private threshold = 1.0

type Net =
    { /// Fixed random input -> hidden weights, hidden-major: neuron h reads input i at h * inputs + i.
      Wiring: float[]
      /// Learned source -> output weights, output-major: output o reads source j at o * sources + j.
      Weights: float[] }

/// Spikes of one move, neuron-major: neuron n fired at step t when Spikes[n * steps + t].
/// Neurons are inputs, then hidden, then outputs (Left, Straight, Right).
type Raster =
    { Spikes: bool[]
      /// Per output: spike count plus the leftover potential squashed into 0 to 1, so silent outputs still rank,
      /// and never above an output with more spikes. Unsquashed, strong inhibition could push it below.
      Scores: float[]
      /// Spikes per source this move, for learning.
      Counts: float[] }

/// Inputs fire by rate: a sense of strength v adds v to a phase each step and fires each time it passes 1.
/// Regular, not random, so a frozen snapshot replays the same.
let run (net: Net) (senses: float[]) : Raster =
    let spikes = Array.zeroCreate<bool> (neurons * steps)
    let counts = Array.zeroCreate<float> sources
    let drive =
        Array.init inputs (fun i ->
            if i < inputCount then max 0.0 (at i senses) else max 0.0 (-(at (at (i - inputCount) negative) senses)))
    let phase = Array.zeroCreate<float> inputs
    let potential = Array.zeroCreate<float> (hidden + outputs)
    let fired = Array.zeroCreate<bool> sources
    let outputCounts = Array.zeroCreate<float> outputs

    for t in 0 .. steps - 1 do
        for i in 0 .. inputs - 1 do
            phase[i] <- at i phase + at i drive
            let spike = at i phase >= 1.0
            if spike then phase[i] <- at i phase - 1.0
            fired[i] <- spike

        // Hidden neurons read this step's input spikes.
        for h in 0 .. hidden - 1 do
            let mutable v = leak * at h potential

            for i in 0 .. inputs - 1 do
                if at i fired then
                    v <- v + at (h * inputs + i) net.Wiring

            let spike = v >= threshold
            potential[h] <- if spike then 0.0 else v
            fired[inputs + h] <- spike

        for o in 0 .. outputs - 1 do
            let mutable v = leak * at (hidden + o) potential

            for j in 0 .. sources - 1 do
                if at j fired then
                    v <- v + at (o * sources + j) net.Weights

            let spike = v >= threshold
            potential[hidden + o] <- if spike then 0.0 else v

            if spike then
                outputCounts[o] <- at o outputCounts + 1.0
                spikes[(sources + o) * steps + t] <- true

        for j in 0 .. sources - 1 do
            if at j fired then
                counts[j] <- at j counts + 1.0
                spikes[j * steps + t] <- true

    { Spikes = spikes
      Scores =
        Array.init outputs (fun o ->
            let v = at (hidden + o) potential
            at o outputCounts + (1.0 + v / (1.0 + abs v)) / 2.0)
      Counts = counts }

let private turns = [| TurnLeft; Straight; TurnRight |]

/// The output with the highest score. Ties go straight first, like QTable.greedy.
let decide (r: Raster) =
    let mutable best = 1

    for o in [ 0; 2 ] do
        if r.Scores[o] > r.Scores[best] then
            best <- o

    turns[best]

/// Each hidden neuron listens to a few random inputs, mostly exciting, some inhibiting, so it fires for
/// particular combinations of senses. Those combinations are what a single layer of output synapses cannot form.
let private wiringInputs = 6

let random (rng: Source) =
    let wiring = Array.zeroCreate<float> (hidden * inputs)

    for h in 0 .. hidden - 1 do
        for _ in 1..wiringInputs do
            wiring[h * inputs + rng.Int inputs] <- rng.Float() * 1.5 - 0.5

    { Wiring = wiring
      Weights = Array.zeroCreate (outputs * sources) }

/// What the learner carries between moves.
type Brain =
    { Net: Net
      /// Eligibility trace per output synapse: recent input activity, signed by whether that output was chosen.
      Trace: float[]
      /// The last move's spikes per source and choice probabilities, kept from act to learn.
      mutable Counts: float[]
      mutable Policy: float[]
      /// Running average dopamine. Learning uses the surprise: dopamine minus this.
      mutable Baseline: float }

let private temperature = 1.0
let private learningRate = 0.01
let private baselineRate = 0.01
/// Trace kept per move, so dopamine credits the last few moves, most of all the latest.
let private traceDecay = 0.8
let private maxWeight = 2.0

/// Dopamine per move. Fruit and death pulse, approach gives a small signal before the first fruit,
/// like the approach credit in Evolution.trial.
let private dopamine =
    function
    | Ate -> Some 1.0
    | Died -> Some -1.0
    | Closer -> Some 0.1
    | Farther -> Some -0.1
    | Starvation -> None

let start (rng: Source) =
    { Net = random rng
      Trace = Array.zeroCreate (outputs * sources)
      Counts = Array.zeroCreate sources
      Policy = Array.create outputs (1.0 / float outputs)
      Baseline = 0.0 }

/// Picks at random, weighted by a softmax of the output scores. Learning needs choices that are sometimes not
/// the top score: with the greedy choice, Left won every tie with Right and took all the learning.
let act (rng: Source) (b: Brain) (g: Game) =
    let r = run b.Net (vision g)
    let top = Array.max r.Scores
    let e = r.Scores |> Array.map (fun s -> exp ((s - top) / temperature))
    let total = Array.sum e
    b.Policy <- e |> Array.map (fun x -> x / total)
    b.Counts <- r.Counts
    let u = rng.Float()
    turns[if u < b.Policy[0] then 0 elif u < b.Policy[0] + b.Policy[1] then 1 else 2]

/// Decays every trace and adds this move's input activity: up for the chosen output, down for the others by
/// how likely they were (REINFORCE). Dopamine above its running average then strengthens the traced synapses,
/// below it weakens them.
let learn (b: Brain) (before: Game) (turn: Turn) (after: Step) =
    let o =
        match turn with
        | TurnLeft -> 0
        | Straight -> 1
        | TurnRight -> 2

    for p in 0 .. outputs - 1 do
        let post = (if p = o then 1.0 else 0.0) - at p b.Policy

        for j in 0 .. sources - 1 do
            let k = p * sources + j
            b.Trace[k] <- traceDecay * at k b.Trace + post * at j b.Counts / float steps

    match dopamine (feedback before after) with
    | None -> ()
    | Some d ->
        let surprise = d - b.Baseline
        b.Baseline <- b.Baseline + baselineRate * (d - b.Baseline)
        let w = b.Net.Weights

        for k in 0 .. w.Length - 1 do
            w[k] <- max (-maxWeight) (min maxWeight (at k w + learningRate * surprise * at k b.Trace))

    // A new game starts with no memory of the last one's moves.
    match after with
    | Alive _ -> ()
    | Dead _
    | Won _ -> Array.fill b.Trace 0 b.Trace.Length 0.0

/// The wiring never changes, so the snapshot shares it.
let freeze (b: Brain) =
    { b.Net with Weights = Array.copy b.Net.Weights }
