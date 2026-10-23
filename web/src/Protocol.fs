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
/// Circuit holds the connectome JSON in fruit fly and worm modes. The page already loaded and validated it,
/// so the worker needs no fetch and no error path of its own. Up to 100 KB, sent only on page actions.
type Order =
    {| Run: int
       Seed: uint32
       Running: bool
       Circuit: string option |}

type WiredGenes =
    {| Encoder: float[][]
       Gain: float[]
       Threshold: float[] |}

/// One finished generation. Run lets the page drop reports from a population it already replaced.
/// Exactly one of Net and Wired is set, matching the run's species.
type Report =
    {| Run: int
       Generation: int
       Seed: uint32
       Score: int
       Net: float[][][] option
       Wired: WiredGenes option |}

let private report run (c: Champion<_>) net wired : Report =
    {| Run = run
       Generation = c.Generation
       Seed = c.Seed
       Score = c.Score
       Net = net
       Wired = wired |}

let reportNet run (c: Champion<Brain.Brain>) = report run c (Some c.Brain) None

let reportWired run (c: Champion<Connectome.Genome>) =
    report
        run
        c
        None
        (Some
            {| Encoder = c.Brain.Encoder
               Gain = c.Brain.Gain
               Threshold = c.Brain.Threshold |})

/// None when a connectome report arrives without a loaded circuit. The page only starts those runs after loading it.
let champion (circuit: Connectome.Circuit option) (r: Report) : Champion<Mind> option =
    let mind =
        match r.Net, r.Wired, circuit with
        | Some brain, _, _ -> Some(Net brain)
        | None, Some genes, Some c ->
            Some(
                Wired(
                    c,
                    { Encoder = genes.Encoder
                      Gain = genes.Gain
                      Threshold = genes.Threshold }
                )
            )
        | _ -> None

    mind
    |> Option.map (fun m ->
        { Generation = r.Generation
          Brain = m
          Seed = r.Seed
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
              Steps = if jsTypeof raw?steps = "number" then unbox<int> raw?steps else -1
              Seed =
                if isNull raw?seed then
                    None
                else
                    Some
                        { Encoder = list raw?seed?encoder |> Array.map numbers
                          Gain = numbers raw?seed?gain
                          Threshold = numbers raw?seed?threshold } }
    with e ->
        Error e.Message
