/// Wires the training worker, replay, and the page controls together.
module SnakeEvo.App

open Browser.Dom
open Browser.Types
open Fable.Core
open SnakeEvo.Game
open SnakeEvo.Evolution
open SnakeEvo.Protocol

[<Emit("performance.now()")>]
let private now () : float = jsNative

/// The two Worker members this page uses. Saves a dependency on Fable.Browser.Worker.
type private TrainingWorker =
    abstract postMessage: Order -> unit
    abstract onmessage: ({| data: Report |} -> unit) with get, set

/// Vite bundles a worker only when it finds this exact `new Worker(new URL(..))` form in the source.
[<Emit("new Worker(new URL('./Worker.js', import.meta.url), { type: 'module' })")>]
let private startWorker () : TrainingWorker = jsNative

/// Training pauses every this many generations, so an idle tab stops burning CPU.
/// Scores plateau around generation 100 to 150 in local runs.
let private pauseEvery = 300

/// Pause after a replay ends, so the death is visible.
let private endHoldMs = 1200.0

type private Training =
    | Running
    | Paused

/// Which champion plays next.
type private Pick =
    | Latest
    | Fixed of generation: int

type private Finish =
    | Died of Death
    | Filled

type private Replay =
    { Champion: Champion
      Game: Game
      /// Activations for the current position, which decide the next move.
      Acts: float[][] }

type private Show =
    | Waiting
    | Playing of Replay
    | Ended of Replay * Finish * at: float

let private element id = document.getElementById id
let private canvas id = element id :?> HTMLCanvasElement

let private boardCanvas = canvas "board"
let private networkCanvas = canvas "network"
let private chartCanvas = canvas "chart"
let private trainButton = element "train" :?> HTMLButtonElement
let private followBox = element "follow" :?> HTMLInputElement
let private speedSelect = element "speed" :?> HTMLSelectElement
let private restartButton = element "restart" :?> HTMLButtonElement

let private newSeed () = uint32 (JS.Math.random () * 4294967296.0)

let private worker = startWorker ()
let mutable private run = 0
let private champions = ResizeArray<Champion>()
let mutable private training = Running
let mutable private nextPause = pauseEvery
let mutable private pick = Latest
let mutable private show = Waiting
let mutable private lastMove = 0.0

let private moveMs () = 160.0 / float speedSelect.value

let private begin' (c: Champion) =
    let g = Game.start c.Seed

    Playing
        { Champion = c
          Game = g
          Acts = Brain.think c.Brain (vision g) }

let private chosen (r: Replay) = Brain.decide (Array.last r.Acts)

let private choiceIndex =
    function
    | TurnLeft -> 0
    | Straight -> 1
    | TurnRight -> 2

let private nextChampion () =
    match show, pick with
    // Every visitor first sees generation 1 crash, for contrast with what follows.
    | Waiting, _ -> Seq.tryHead champions
    | _, Latest -> Seq.tryLast champions
    | _, Fixed gen -> Some champions[gen - 1]

let private advance t =
    match show with
    | Waiting ->
        match nextChampion () with
        | Some c ->
            show <- begin' c
            lastMove <- t
        | None -> ()
    | Playing r when t - lastMove >= moveMs () ->
        lastMove <- t

        show <-
            match step (chosen r) r.Game with
            | Alive g -> Playing { r with Game = g; Acts = Brain.think r.Champion.Brain (vision g) }
            | Dead(death, _) -> Ended(r, Died death, t)
            | Won g -> Ended({ r with Game = g }, Filled, t)
    | Ended(_, _, at) when t - at >= endHoldMs ->
        nextChampion () |> Option.iter (fun c -> show <- begin' c)
        lastMove <- t
    | Playing _
    | Ended _ -> ()

let private setText (el: HTMLElement) (text: string) =
    if el.textContent <> text then
        el.textContent <- text

let private finishLabel =
    function
    | Died HitWall -> "Hit wall"
    | Died HitSelf -> "Hit itself"
    | Died Starved -> "Starved"
    | Filled -> "Board full"

let private render () =
    let replay =
        match show with
        | Waiting -> None
        | Playing r
        | Ended(r, _, _) -> Some r

    match replay with
    | Some r ->
        View.board boardCanvas r.Game r.Acts[0]
        View.network networkCanvas r.Champion.Brain r.Acts (choiceIndex (chosen r))
        setText (element "generation") (string r.Champion.Generation)
        setText (element "score") (string r.Game.Score)
    | None -> ()

    setText
        (element "outcome")
        (match show with
         | Ended(_, finish, _) -> finishLabel finish
         | Playing _ -> "Playing"
         | Waiting -> "Training")

    setText (element "trained") (string champions.Count)

    let current = replay |> Option.map (fun r -> r.Champion.Generation)
    View.chart chartCanvas (champions.ToArray()) current
    chartCanvas.setAttribute ("aria-valuemax", string (max 1 champions.Count))
    current |> Option.iter (fun g -> chartCanvas.setAttribute ("aria-valuenow", string g))

    setText
        trainButton
        (match training with
         | Running -> "Pause"
         | Paused -> "Resume")

let private send () =
    let order: Order =
        {| Run = run
           Seed = newSeed ()
           Running = (training = Running) |}

    worker.postMessage order

worker.onmessage <-
    fun e ->
        let r = e.data

        // Reports from a replaced population can still be in flight after Restart.
        if r.Run = run then
            champions.Add(champion r)

            if champions.Count >= nextPause && training = Running then
                training <- Paused
                nextPause <- champions.Count + pauseEvery
                send ()

let rec private frame (t: float) =
    advance t
    render ()
    window.requestAnimationFrame frame |> ignore

/// Replays a chosen generation now and stops following the latest.
let private select gen =
    if gen >= 1 && gen <= champions.Count then
        pick <- Fixed gen
        followBox.``checked`` <- false
        show <- begin' champions[gen - 1]
        lastMove <- now ()

trainButton.onclick <-
    fun _ ->
        training <-
            match training with
            | Running -> Paused
            | Paused -> Running

        send ()

followBox.onchange <-
    fun _ ->
        if followBox.``checked`` then
            pick <- Latest
            Seq.tryLast champions |> Option.iter (fun c -> show <- begin' c)
        else
            match show with
            | Playing r
            | Ended(r, _, _) -> pick <- Fixed r.Champion.Generation
            | Waiting -> ()

restartButton.onclick <-
    fun _ ->
        run <- run + 1
        champions.Clear()
        training <- Running
        nextPause <- pauseEvery
        pick <- Latest
        followBox.``checked`` <- true
        show <- Waiting
        send ()

chartCanvas.onclick <-
    fun e ->
        View.generationAt chartCanvas champions.Count e.offsetX
        |> Option.iter select

chartCanvas.onkeydown <-
    fun e ->
        let current =
            match show with
            | Playing r
            | Ended(r, _, _) -> r.Champion.Generation
            | Waiting -> 0

        match e.key with
        | "ArrowLeft" -> select (current - 1); e.preventDefault ()
        | "ArrowRight" -> select (current + 1); e.preventDefault ()
        | "Home" -> select 1; e.preventDefault ()
        | "End" -> select champions.Count; e.preventDefault ()
        | _ -> ()

send ()
window.requestAnimationFrame frame |> ignore
