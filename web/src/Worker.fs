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

type private Session =
    | Idle
    | Active of run: int * Trainer * running: bool

let mutable private session = Idle
let mutable private scheduled = false

let rec private work () =
    scheduled <- false

    match session with
    | Active(run, trainer, true) ->
        let deadline = now () + sliceMs

        for c in trainer.Train(fun () -> now () < deadline) do
            post (report run c)

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
        | Active(run, trainer, _) when run = order.Run -> Active(run, trainer, order.Running)
        | Active _
        | Idle -> Active(order.Run, Trainer order.Seed, order.Running)

    schedule ())
