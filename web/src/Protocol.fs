/// Messages between the page and the training worker, and the circuit file format.
/// Anonymous records compile to plain JS objects. Structured clone drops class prototypes,
/// so F# records and unions would arrive broken.
module SnakeEvo.Protocol

open Fable.Core
open Fable.Core.JsInterop
open SnakeEvo.Evolution
open SnakeEvo.Mind

/// Desired worker state, sent whole. Each order is idempotent, so no command can arrive out of context.
/// A new Run starts a fresh population from Seed.
/// Species is "net", "neat", "program", "qtable", "spiking", "circuit", or "none". None stops training: the Hamiltonian cycle
/// mode does not learn. A string, because structured clone would break a union.
/// Circuit holds the connectome JSON in fruit fly and worm modes. The page already loaded and validated it,
/// so the worker needs no fetch and no error path of its own. Up to 100 KB, sent only on page actions.
type Order =
    {| Run: int
       Seed: uint32
       Running: bool
       Species: string
       Circuit: string option |}

type WiredGenes =
    {| Encoder: float[][]
       Gain: float[]
       Threshold: float[] |}

/// NEAT links as parallel arrays, in genome order. Compiling sums links in that order, so keeping it makes
/// the page's net move exactly like the worker's.
type NeatGenes =
    {| From: int[]
       To: int[]
       Weight: float[]
       Enabled: bool[] |}

/// The fixed wiring travels with every snapshot. About 2,000 numbers, small next to the time a game takes.
type SpikingGenes = {| Wiring: float[]; Weights: float[] |}

/// One finished generation. Run lets the page drop reports from a population it already replaced.
/// Exactly one genome field is set, matching the run's species. Program is the tree in Program.encode form.
type Report =
    {| Run: int
       Generation: int
       Score: int
       Net: float[][][] option
       Wired: WiredGenes option
       Neat: NeatGenes option
       Program: int[] option
       Table: float[] option
       Spiking: SpikingGenes option |}

/// A report with no genome yet. Each species fills in its own field.
let private empty run (c: Champion<_>) : Report =
    {| Run = run
       Generation = c.Generation
       Score = c.Score
       Net = None
       Wired = None
       Neat = None
       Program = None
       Table = None
       Spiking = None |}

let reportNet run (c: Champion<Brain.Brain>) =
    {| empty run c with Net = Some c.Brain |}

let reportWired run (c: Champion<Connectome.Genome>) =
    {| empty run c with
        Wired =
            Some
                {| Encoder = c.Brain.Encoder
                   Gain = c.Brain.Gain
                   Threshold = c.Brain.Threshold |} |}

let reportNeat run (c: Champion<Neat.Genome>) =
    let links = c.Brain

    {| empty run c with
        Neat =
            Some
                {| From = links |> Array.map (fun l -> l.From)
                   To = links |> Array.map (fun l -> l.To)
                   Weight = links |> Array.map (fun l -> l.Weight)
                   Enabled = links |> Array.map (fun l -> l.Enabled) |} |}

let reportProgram run (c: Champion<Program.Tree>) =
    {| empty run c with Program = Some(Program.encode c.Brain) |}

let reportTable run (c: Champion<QTable.Table>) = {| empty run c with Table = Some c.Brain |}

let reportSpiking run (c: Champion<Spiking.Net>) =
    {| empty run c with
        Spiking =
            Some
                {| Wiring = c.Brain.Wiring
                   Weights = c.Brain.Weights |} |}

/// None when a connectome report arrives without a loaded circuit. The page only starts those runs after loading it.
let champion (circuit: Connectome.Circuit option) (r: Report) : Champion<Mind> option =
    let wired =
        match r.Wired, circuit with
        | Some genes, Some c ->
            Some(
                Wired(
                    c,
                    { Encoder = genes.Encoder
                      Gain = genes.Gain
                      Threshold = genes.Threshold }
                )
            )
        | _ -> None

    let grown (genes: NeatGenes) =
        Grown(
            Neat.compile (
                Array.init genes.From.Length (fun i ->
                    { From = genes.From[i]
                      To = genes.To[i]
                      Weight = genes.Weight[i]
                      Enabled = genes.Enabled[i] }: Neat.Link)
            )
        )

    [ r.Net |> Option.map Net
      wired
      r.Neat |> Option.map grown
      r.Program |> Option.map (Program.decode >> Rules)
      r.Table |> Option.map Tabular
      r.Spiking
      |> Option.map (fun n ->
          Mind.Spiking
              { Wiring = n.Wiring
                Weights = n.Weights }) ]
    |> List.tryPick id
    |> Option.map (fun m ->
        { Generation = r.Generation
          Brain = m
          Score = r.Score })

/// Parses a circuit file (web/data/*-circuit.json, see web/tools). Shape checks here, index checks in Connectome.build.
let parseCircuit (text: string) : Result<Connectome.Circuit, string> =
    try
        let raw = JS.JSON.parse text

        let numbers (o: obj) =
            if not (JS.Constructors.Array.isArray o) then
                failwith "expected an array"

            (o :?> obj[])
            |> Array.map (fun v ->
                if jsTypeof v = "number" then
                    unbox<float> v
                else
                    failwith "expected a number")

        let list (o: obj) =
            if JS.Constructors.Array.isArray o then
                o :?> obj[]
            else
                failwith "expected an array"

        let pairs (o: obj) =
            list o
            |> Array.map (fun p ->
                match numbers p with
                | [| i; w |] -> int i, w
                | _ -> failwith "expected neuron, weight pairs")

        let neurons =
            list raw?neurons
            |> Array.map (fun n ->
                match numbers n?pos with
                | [| x; y |] -> string n?``type``, string n?side, string n?role, x, y
                | _ -> failwith "neuron position must be x, y")

        Connectome.build
            { Neurons = neurons
              Inputs = numbers raw?inputs |> Array.map int
              Left = pairs raw?left
              Right = pairs raw?right
              Edges = numbers raw?edges
              // The fly file has no gap junctions.
              Gaps = if isNull raw?gaps then [||] else numbers raw?gaps
              Cloud = numbers raw?cloud
              Steps = if jsTypeof raw?steps = "number" then unbox<int> raw?steps else -1 }
    with e ->
        Error e.Message
