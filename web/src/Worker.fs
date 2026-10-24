/// Trains off the main thread, so training gets a full core and the animation never stutters.
/// Nothing may import this module: its top-level code installs the worker message handler.
module SnakeEvo.Worker

open Fable.Core
open SnakeEvo.Evolution
open SnakeEvo.Protocol

[<Emit("performance.now()")>]
let private now () : float = jsNative

[<Emit("self.postMessage($0)")>]
let private post (report: Report) : unit = jsNative

[<Emit("self.onmessage = (e) => $0(e.data)")>]
let private onOrder (handle: Order -> unit) : unit = jsNative

[<Emit("setTimeout($0, 0)")>]
let private soon (work: unit -> unit) : unit = jsNative

/// Training time between checks for new orders. Long enough that the ~4 ms setTimeout clamp costs little.
let private sliceMs = 100.0

/// Trains for one slice and returns the reports. Hides the species, so one session type serves both.
type private Train = (unit -> bool) -> Report list

type private Session =
    | Idle
    | Active of run: int * Train * running: bool

let private start (order: Order) : Train =
    match order.Species, order.Circuit with
    | "net", _ ->
        let trainer = Trainer(order.Seed, net)
        fun hasTime -> trainer.Train hasTime |> List.map (reportNet order.Run)
    | "neat", _ ->
        let trainer = Trainer(order.Seed, neat ())
        fun hasTime -> trainer.Train hasTime |> List.map (reportNeat order.Run)
    | "program", _ ->
        let trainer = Trainer(order.Seed, program)
        fun hasTime -> trainer.Train hasTime |> List.map (reportProgram order.Run)
    | "circuit", Some text ->
        match parseCircuit text with
        | Ok circuit ->
            let trainer = Trainer(order.Seed, connectome circuit)
            fun hasTime -> trainer.Train hasTime |> List.map (reportWired order.Run)
        // The page parsed this exact text before sending it. Throwing fires the worker's error event on the page.
        | Error message -> failwith $"circuit rejected: {message}"
    | "circuit", None -> failwith "circuit order without a circuit"
    | species, _ -> failwith $"unknown species {species}"

let mutable private session = Idle
let mutable private scheduled = false

let rec private work () =
    scheduled <- false

    match session with
    | Active(_, train, true) ->
        let deadline = now () + sliceMs

        for report in train (fun () -> now () < deadline) do
            post report

        schedule ()
    | Active(_, _, false)
    | Idle -> ()

and private schedule () =
    if not scheduled then
        scheduled <- true
        soon work

onOrder (fun order ->
    session <-
        match session with
        | _ when order.Species = "none" -> Idle
        | Active(run, train, _) when run = order.Run -> Active(run, train, order.Running)
        | Active _
        | Idle -> Active(order.Run, start order, order.Running)

    schedule ())
