/// NEAT (NeuroEvolution of Augmenting Topologies, Stanley and Miikkulainen 2002): evolution grows the network.
/// It starts with every input wired straight to the outputs, then adds links and splits links into new neurons.
/// TODO: no speciation, so a new neuron must pay off at once or selection drops it. Growth does not help yet: a
/// control without new links or neurons scored the same (35 fruit at generation 140, 4 seeds). Revisit if growth
/// should earn its keep: add NEAT's species with fitness sharing inside Evolution.Trainer, then rerun that control.
module SnakeEvo.Neat

open System.Collections.Generic
open SnakeEvo.Native
open SnakeEvo.Rng
open SnakeEvo.Game

/// Node ids: snake inputs 0 to 26, bias 27, outputs 28 to 30 (turn left, straight, right), hidden from 31.
let bias = inputCount
let firstOutput = inputCount + 1
let outputCount = 3
let firstHidden = firstOutput + outputCount

let private isOutput id = id >= firstOutput && id < firstHidden

/// A link is identified by its From, To pair. Crossover aligns parents by it, so it plays the role of
/// NEAT's innovation number without a global counter.
type Link =
    { From: int
      To: int
      Weight: float
      Enabled: bool }

/// Links are never removed, only disabled, so every hidden node stays an endpoint of some link.
/// No cycle exists over all links, enabled or not, so re-enabling a link is always safe.
type Genome = Link[]

/// Hands out hidden node ids. Splitting the same link in two lineages gives the same node, so crossover
/// can align the new links. One registry per training run: the experiment script runs seeds in parallel.
type Innovations() =
    let splits = Dictionary<string, int>()
    let mutable next = firstHidden

    member _.Split(from: int, ``to``: int) =
        let key = $"{from}>{``to``}"

        match splits.TryGetValue key with
        | true, id -> id
        | _ ->
            let id = next
            next <- next + 1
            splits[key] <- id
            id

let private mutationChance = 0.10
let private addLinkChance = 0.15
let private addNodeChance = 0.05
let private enableChance = 0.02
let private addLinkTries = 20

let private randomWeight (rng: Source) = rng.Float() * 2.0 - 1.0

/// Every input and the bias wired to every output: 84 links. A sparse start leaves outputs unconnected,
/// and an all-zero output always picks the first choice (Brain.decide), so the first generations would
/// mostly play one constant move and selection would have little to work with.
let random (rng: Source) : Genome =
    [| for o in firstOutput .. firstHidden - 1 do
           for i in 0..bias ->
               { From = i
                 To = o
                 Weight = randomWeight rng
                 Enabled = true } |]

/// True when `target` is reachable from `source` over all links. Adding source <- target would close a cycle.
let private reaches (links: Link[]) source target =
    let seen = HashSet<int>()
    let mutable stack = [ source ]
    let mutable found = false

    while not found && not stack.IsEmpty do
        let node = stack.Head
        stack <- stack.Tail

        if node = target then found <- true
        elif seen.Add node then
            for l in links do
                if l.From = node then
                    stack <- l.To :: stack

    found

let private nodesOf (links: Link[]) =
    links |> Array.collect (fun l -> [| l.From; l.To |]) |> Array.distinct

let private addLink (rng: Source) (links: Link[]) =
    let hidden = nodesOf links |> Array.filter (fun id -> id >= firstHidden)
    let sources = Array.append [| 0..bias |] hidden
    let targets = Array.append [| firstOutput .. firstHidden - 1 |] hidden

    let rec attempt tries =
        if tries = 0 then
            links
        else
            let from = sources[rng.Int sources.Length]
            let ``to`` = targets[rng.Int targets.Length]

            if from = ``to`` || links |> Array.exists (fun l -> l.From = from && l.To = ``to``) || reaches links ``to`` from then
                attempt (tries - 1)
            else
                Array.append
                    links
                    [| { From = from
                         To = ``to``
                         Weight = randomWeight rng
                         Enabled = true } |]

    attempt addLinkTries

/// Splits an enabled link a -> b into a -> new -> b. The new links carry weight 1 and the old weight.
/// Hidden nodes use tanh, which is near linear for small sums, so the split barely changes behavior at first.
let private addNode (rng: Source) (innovations: Innovations) (links: Link[]) =
    let enabled = links |> Array.indexed |> Array.filter (fun (_, l) -> l.Enabled)

    if enabled.Length = 0 then
        links
    else
        let k, old = enabled[rng.Int enabled.Length]
        let id = innovations.Split(old.From, old.To)

        // This lineage already split the link once, then re-enabled it. A second copy would duplicate links.
        if links |> Array.exists (fun l -> l.From = id || l.To = id) then
            links
        else
            Array.append
                (links |> Array.mapi (fun i l -> if i = k then { l with Enabled = false } else l))
                [| { From = old.From
                     To = id
                     Weight = 1.0
                     Enabled = true }
                   { From = id
                     To = old.To
                     Weight = old.Weight
                     Enabled = true } |]

let private enableOne (rng: Source) (links: Link[]) =
    let disabled = links |> Array.indexed |> Array.filter (fun (_, l) -> not l.Enabled)

    if disabled.Length = 0 then
        links
    else
        let k, _ = disabled[rng.Int disabled.Length]
        links |> Array.mapi (fun i l -> if i = k then { l with Enabled = true } else l)

/// The child keeps parent a's links, so it inherits a's acyclic structure. Links both parents share take
/// a random parent's weight and enabled flag. Links only a has keep a's values. Links only b has are dropped:
/// the trainer passes no fitness, and a is the first rank-biased pick, so it stands in for the fitter parent.
/// Always builds new arrays: elites are shared by reference and can be parents many times.
let crossover (innovations: Innovations) (rng: Source) (a: Genome) (b: Genome) : Genome =
    let other = Dictionary<string, Link>()

    for l in b do
        other[$"{l.From}>{l.To}"] <- l

    let child =
        a
        |> Array.map (fun l ->
            match other.TryGetValue $"{l.From}>{l.To}" with
            | true, m ->
                { l with
                    Weight = Brain.inheritGene rng mutationChance l.Weight m.Weight
                    Enabled = if rng.Float() > 0.5 then l.Enabled else m.Enabled }
            | _ ->
                { l with
                    Weight = Brain.inheritGene rng mutationChance l.Weight l.Weight })

    let child = if rng.Float() < addLinkChance then addLink rng child else child
    let child = if rng.Float() < addNodeChance then addNode rng innovations child else child
    if rng.Float() < enableChance then enableOne rng child else child

/// A genome compiled for fast play. Nodes are indexed in evaluation order: inputs and bias, hidden nodes by
/// depth, outputs last.
type Net =
    { Genome: Genome
      /// Node id per index.
      Ids: int[]
      /// Longest enabled path from an input: inputs 0, outputs share the deepest column.
      Depth: int[]
      /// Enabled links grouped by target index, in genome order: sources from Start[j] to Start[j + 1] - 1.
      Start: int[]
      Source: int[]
      Weight: float[] }

/// Orders nodes so every link points forward. Ties break by node id, and sums run in genome order,
/// so the page rebuilds exactly the net the worker trained, and replays match bit for bit.
let compile (genome: Genome) : Net =
    let enabled = genome |> Array.filter (fun l -> l.Enabled)
    let hidden = nodesOf genome |> Array.filter (fun id -> id >= firstHidden) |> Array.sort
    let incoming = Dictionary<int, ResizeArray<Link>>()

    for l in enabled do
        match incoming.TryGetValue l.To with
        | true, ls -> ls.Add l
        | _ -> incoming[l.To] <- ResizeArray [ l ]

    let depth = Dictionary<int, int>()
    let visiting = HashSet<int>()

    let rec depthOf id =
        if id <= bias then
            0
        else
            match depth.TryGetValue id with
            | true, d -> d
            | _ ->
                // Crossover and mutation keep genomes acyclic. Fail loudly rather than recurse forever.
                if not (visiting.Add id) then
                    failwith $"NEAT genome has a cycle through node {id}"

                let d =
                    match incoming.TryGetValue id with
                    | true, ls -> 1 + (ls |> Seq.map (fun l -> depthOf l.From) |> Seq.max)
                    | _ -> 1

                depth[id] <- d
                d

    let hidden = hidden |> Array.sortBy (fun id -> depthOf id, id)
    let deepest = hidden |> Array.fold (fun m id -> max m (depthOf id)) 0
    let outputs = [| firstOutput .. firstHidden - 1 |]
    let ids = Array.concat [ [| 0..bias |]; hidden; outputs ]
    let index = Dictionary<int, int>()
    ids |> Array.iteri (fun i id -> index[id] <- i)
    let start = Array.zeroCreate<int> (ids.Length + 1)
    let source = ResizeArray<int>()
    let weight = ResizeArray<float>()

    for j in 0 .. ids.Length - 1 do
        start[j] <- source.Count

        match incoming.TryGetValue ids[j] with
        | true, ls ->
            for l in ls do
                source.Add index[l.From]
                weight.Add l.Weight
        | _ -> ()

    start[ids.Length] <- source.Count

    { Genome = genome
      Ids = ids
      Depth = ids |> Array.map (fun id -> if isOutput id then deepest + 1 else depthOf id)
      Start = start
      Source = source.ToArray()
      Weight = weight.ToArray() }

/// Every node's value, by index. Hidden nodes use tanh, outputs stay linear like the network's.
let think (net: Net) (input: float[]) =
    let n = net.Ids.Length
    let value = Array.zeroCreate<float> n

    for i in 0 .. inputCount - 1 do
        value[i] <- at i input

    value[bias] <- 1.0

    for j in bias + 1 .. n - 1 do
        let mutable sum = 0.0

        for e in at j net.Start .. at (j + 1) net.Start - 1 do
            sum <- sum + at e net.Weight * at (at e net.Source) value

        value[j] <- if j >= n - outputCount then sum else tanh sum

    value

let outputs (value: float[]) = value[value.Length - outputCount ..]
