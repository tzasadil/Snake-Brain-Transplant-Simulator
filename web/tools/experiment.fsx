// Short learning runs for comparing brains, settings, and wiring controls. Runs on .NET, whose results match
// the browser exactly (same random numbers, same scores), and much faster.
//
// From web/:
//   dotnet fsi tools/experiment.fsx net
//   dotnet fsi tools/experiment.fsx data/fly-circuit.json
//   dotnet fsi tools/experiment.fsx data/fly-circuit.json random 1,42,5,9 60
//   dotnet fsi tools/experiment.fsx data/fly-circuit.json real 1,42,5,9 60 tuned
//   dotnet fsi tools/experiment.fsx data/fly-circuit.json real 1,42,5,9 60 free
//
// Arguments: brain (net or a circuit file), wiring (real, random, or signs; circuits only),
// comma-separated seeds (default 1,42,5,9), generations (default 60),
// weights (fixed, tuned, or free; circuits only, default fixed).
// Every 20 generations it scores the champion over 20 fixed games. A champion's own training score is a
// lucky maximum, so only these fixed games compare fairly. Seeds run in parallel.
//
// Wiring controls test whether a connectome's wiring matters, or whether evolution makes any circuit work:
//   random: same neurons, inputs, outputs, and edge count. Each edge gets a random sender and receiver, the real
//           weight sizes are shuffled across edges and normalized again per receiving neuron, and each neuron
//           keeps its real sign. Gap junctions are dropped.
//   signs:  real wiring, with excitatory and inhibitory signs shuffled between neurons.
//
// Tuned weights add one gene per chemical edge, as ChessFly does: strength = |w| * exp(theta), so signs stay fixed.
// Theta starts at 0, so generation 1 matches fixed weights exactly. Gap junctions stay fixed.
// Free weights also add a sign gene per edge, starting at the real sign. A mutation flips it outright: with about
// 56 mutations per child over 5,800 genes, each gene mutates about once per lineage in 60 generations, so a small
// Gaussian step on a signed weight would almost never cross 0. This breaks Dale's law (one sign per neuron).
#load "../src/Native.fs" "../src/Rng.fs" "../src/Game.fs" "../src/Brain.fs" "../src/Connectome.fs" "../src/Mind.fs" "../src/Evolution.fs"

open System
open System.IO
open System.Text.Json
open SnakeEvo

let args = fsi.CommandLineArgs |> Array.skip 1
let arg i fallback = if args.Length > i then args[i] else fallback
let brain = arg 0 "net"
let wiring = arg 1 "real"
let seeds = (arg 2 "1,42,5,9").Split ',' |> Array.map uint32
let generations = int (arg 3 "60")
let weights = arg 4 "fixed"

let loadCircuit (path: string) =
    let doc = JsonDocument.Parse(File.ReadAllText path).RootElement
    let numbers (e: JsonElement) = [| for v in e.EnumerateArray() -> v.GetDouble() |]
    let pairs (e: JsonElement) = [| for p in e.EnumerateArray() -> let v = numbers p in int v[0], v[1] |]

    let optional (name: string) =
        match doc.TryGetProperty name with
        | true, e -> numbers e
        | _ -> [||]

    let neurons =
        [| for n in doc.GetProperty("neurons").EnumerateArray() ->
               let p = numbers (n.GetProperty "pos")
               n.GetProperty("type").GetString(), n.GetProperty("side").GetString(), n.GetProperty("role").GetString(), p[0], p[1] |]

    let edges = numbers (doc.GetProperty "edges")
    let n = neurons.Length
    let triples = Array.init (edges.Length / 3) (fun k -> int edges[3 * k], int edges[3 * k + 1], edges[3 * k + 2])
    // Fixed shuffle seed, so a control is the same circuit every run.
    let rnd = Random 12345

    let shuffle (a: 'a[]) =
        let a = Array.copy a

        for i in a.Length - 1 .. -1 .. 1 do
            let j = rnd.Next(i + 1)
            let t = a[i]
            a[i] <- a[j]
            a[j] <- t

        a

    // One sign per sending neuron, as in the extracted files.
    let signOf = Array.create n 1.0

    for pre, _, w in triples do
        signOf[pre] <- (if w < 0.0 then -1.0 else 1.0)

    let normalize (ts: (int * int * float)[]) =
        let total = Array.zeroCreate<float> n

        for _, post, w in ts do
            total[post] <- total[post] + abs w

        ts |> Array.map (fun (pre, post, w) -> pre, post, w / total[post])

    let chosen, gaps =
        match wiring with
        | "real" -> triples, optional "gaps"
        | "random" ->
            let seen = Collections.Generic.HashSet<int * int>()

            let rec pick () =
                let pre, post = rnd.Next n, rnd.Next n
                if pre <> post && seen.Add((pre, post)) then pre, post else pick ()

            let sizes = triples |> Array.map (fun (_, _, w) -> abs w) |> shuffle
            sizes |> Array.map (fun m -> let pre, post = pick () in pre, post, signOf[pre] * m) |> normalize, [||]
        | "signs" ->
            let signs = shuffle signOf
            triples |> Array.map (fun (pre, post, w) -> pre, post, signs[pre] * abs w), optional "gaps"
        | other -> failwith $"unknown wiring {other}: use real, random, or signs"

    match
        Connectome.build
            { Neurons = neurons
              Inputs = numbers (doc.GetProperty "inputs") |> Array.map int
              Left = pairs (doc.GetProperty "left")
              Right = pairs (doc.GetProperty "right")
              Edges = chosen |> Array.collect (fun (a, b, w) -> [| float a; float b; w |])
              Gaps = gaps
              Cloud = [||]
              Steps = doc.GetProperty("steps").GetInt32() }
    with
    | Ok c -> c
    | Error e -> failwith e

/// Mean score over 20 fixed games.
let evaluate (mind: Mind.Mind) =
    [ for s in 1u .. 20u ->
          let mutable step = Game.Alive(Game.start (s * 7919u))

          while (match step with
                 | Game.Alive _ -> true
                 | _ -> false) do
              let g = Game.state step
              step <- Game.step (Mind.think mind (Game.vision g)).Turn g

          float (Game.state step).Score ]
    |> List.average

/// Trains one seed and returns the champion's fixed-game score every 20 generations.
let run (train: uint32 -> (int -> Mind.Mind)) seed =
    let champion = train seed
    [ for gen in 20..20..generations -> gen, evaluate (champion gen) ]

let trainer species toMind seed =
    let t = Evolution.Trainer(seed, species)
    let champions = Collections.Generic.Dictionary<int, Mind.Mind>()

    fun gen ->
        while t.Generation <= gen do
            for c in t.Train(fun () -> t.Generation <= gen) do
                champions[c.Generation] <- toMind c.Brain

        champions[gen]

/// Circuit genome plus one log strength per chemical edge, and with `flips` one sign per edge.
/// Mutation keeps Connectome's 56 per child over all genes.
let tuned flips (c: Connectome.Circuit) : Evolution.Species<Connectome.Genome * float[] * float[]> =
    let toMind (g, theta: float[], sign: float[]) =
        // Scaled once per genome, not per move. Sign starts at sign(w), so w * sign is |w|.
        Mind.Wired({ c with Weight = Array.init c.Weight.Length (fun e -> c.Weight[e] * sign[e] * exp theta[e]) }, g)

    { Random = fun rng -> Connectome.random c rng, Array.zeroCreate c.Weight.Length, Array.create c.Weight.Length 1.0
      Crossover =
        fun rng (a, ta, sa) (b, tb, sb) ->
            let genes =
                a.Encoder.Length * a.Encoder[0].Length + a.Gain.Length + a.Threshold.Length + ta.Length
                + (if flips then sa.Length else 0)

            let chance = 56.0 / float genes
            let gene = Brain.inheritGene rng chance

            let flip s1 s2 =
                let s = if rng.Float() > 0.5 then s1 else s2
                if rng.Float() > chance then s else -s

            { Connectome.Encoder = Array.map2 (Array.map2 gene) a.Encoder b.Encoder
              Connectome.Gain = Array.map2 gene a.Gain b.Gain
              Connectome.Threshold = Array.map2 gene a.Threshold b.Threshold },
            Array.map2 gene ta tb,
            (if flips then Array.map2 flip sa sb else sa)
      Mind = toMind }

let train =
    match brain, weights with
    | "net", _ -> trainer Evolution.net Mind.Net
    | _, "fixed" ->
        let circuit = loadCircuit brain
        trainer (Evolution.connectome circuit) (fun g -> Mind.Wired(circuit, g))
    | _, ("tuned" | "free") ->
        let species = tuned (weights = "free") (loadCircuit brain)
        trainer species species.Mind
    | _, other -> failwith $"unknown weights {other}: use fixed, tuned, or free"

let results = seeds |> Array.Parallel.map (fun seed -> seed, run train seed)

for seed, scores in results do
    printfn "%s %s %s seed %d: %s" (Path.GetFileName brain) wiring weights seed (scores |> List.map (fun (g, s) -> $"g{g} {s:F1}") |> String.concat "  ")

let final = results |> Array.map (fun (_, scores) -> scores |> List.last |> snd)
printfn "mean at generation %d: %.1f" generations (Array.average final)
