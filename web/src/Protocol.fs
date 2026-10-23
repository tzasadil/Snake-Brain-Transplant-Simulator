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
/// FlyCircuit holds the circuit JSON in fruit fly mode. The page already loaded and validated it,
/// so the worker needs no fetch and no error path of its own. About 120 KB, sent only on page actions.
type Order =
    {| Run: int
       Seed: uint32
       Running: bool
       FlyCircuit: string option |}

type FlyGenes =
    {| Encoder: float[][]
       Gain: float[]
       Threshold: float[] |}

/// One finished generation. Run lets the page drop reports from a population it already replaced.
/// Exactly one of Net and Fly is set, matching the run's species.
type Report =
    {| Run: int
       Generation: int
       Seed: uint32
       Score: int
       Net: float[][][] option
       Fly: FlyGenes option |}

let private report run (c: Champion<_>) net fly : Report =
    {| Run = run
       Generation = c.Generation
       Seed = c.Seed
       Score = c.Score
       Net = net
       Fly = fly |}

let reportNet run (c: Champion<Brain.Brain>) = report run c (Some c.Brain) None

let reportFly run (c: Champion<Fly.Genome>) =
    report
        run
        c
        None
        (Some
            {| Encoder = c.Brain.Encoder
               Gain = c.Brain.Gain
               Threshold = c.Brain.Threshold |})

/// None when a fly report arrives without a loaded circuit. The page only starts fly runs after loading it.
let champion (circuit: Fly.Circuit option) (r: Report) : Champion<Mind> option =
    let mind =
        match r.Net, r.Fly, circuit with
        | Some brain, _, _ -> Some(Net brain)
        | None, Some genes, Some c ->
            Some(
                Fly(
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

/// Parses web/public/fly-circuit.json (see web/tools/extract_fly.py). Shape checks here, index checks in Fly.build.
let parseCircuit (text: string) : Result<Fly.Circuit, string> =
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

        Fly.build
            neurons
            (numbers raw?inputs |> Array.map int)
            (pairs raw?left)
            (pairs raw?right)
            (numbers raw?edges)
            (numbers raw?cloud)
    with e ->
        Error e.Message
