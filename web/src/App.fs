/// Wires the training worker, playback, and the page controls together.
module SnakeEvo.App

open Browser.Dom
open Browser.Types
open Fable.Core
open SnakeEvo.Game
open SnakeEvo.Evolution
open SnakeEvo.Protocol

[<Emit("performance.now()")>]
let private now () : float = jsNative

/// The Worker members this page uses. Saves a dependency on Fable.Browser.Worker.
type private TrainingWorker =
    abstract postMessage: Order -> unit
    abstract onmessage: ({| data: Report |} -> unit) with get, set
    abstract onerror: (obj -> unit) with get, set

[<Emit("fetch($0).then(r => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.text(); })")>]
let private fetchText (url: string) : JS.Promise<string> = jsNative

// Vite fingerprints a file it finds in this exact `new URL('..', import.meta.url)` form, so each deploy
// gets a new file name. Plain fetches of fixed names let browsers mix a cached old circuit file with new code,
// which the parser then rejects. The paths are relative to the compiled build/App.js.
[<Emit("new URL('../data/fly-circuit.json', import.meta.url).href")>]
let private flyUrl () : string = jsNative

[<Emit("new URL('../data/worm-circuit.json', import.meta.url).href")>]
let private wormUrl () : string = jsNative

/// Vite bundles a worker only when it finds this exact `new Worker(new URL(..))` form in the source.
[<Emit("new Worker(new URL('./Worker.js', import.meta.url), { type: 'module' })")>]
let private startWorker () : TrainingWorker = jsNative

/// Training pauses every this many generations, so an idle tab stops burning CPU. Resume continues.
let private pauseEvery = 300

/// Pause after a game ends, so the death is visible.
let private endHoldMs = 1200.0

type private Training =
    | Running
    | Paused
    /// The worker threw. Restart sends a fresh order.
    | Failed

type private CircuitData =
    | Loading
    /// The raw text goes to the worker, the parsed circuit stays here for playback.
    | Ready of text: string * Connectome.Circuit
    | Unavailable

/// Brains wired by a real connectome. Each loads its circuit file after the page.
type private Kind =
    | Fly
    | Worm

type private Species =
    | NetSpecies
    | NeatSpecies
    | ProgramSpecies
    | WiredSpecies of Kind

/// Dropdown value, circuit file, and legend element of each wired kind.
let private info =
    function
    | Fly -> {| Value = "fly"; Url = flyUrl; Name = "Fruit fly"; Legend = "legend-fly" |}
    | Worm -> {| Value = "worm"; Url = wormUrl; Name = "Worm"; Legend = "legend-worm" |}

let private kinds = [ Fly; Worm ]
let private allSpecies = NetSpecies :: NeatSpecies :: ProgramSpecies :: List.map WiredSpecies kinds

/// Dropdown value and legend element of each species.
let private describe =
    function
    | NetSpecies -> {| Value = "net"; Legend = "legend-net" |}
    | NeatSpecies -> {| Value = "neat"; Legend = "legend-neat" |}
    | ProgramSpecies -> {| Value = "program"; Legend = "legend-program" |}
    | WiredSpecies k -> {| Value = (info k).Value; Legend = (info k).Legend |}

/// Which champion plays next.
type private Pick =
    /// Each new champion takes over the game in progress, from the first generation on.
    /// An earlier intro played generation 1 to its death first, but a fly's first brain
    /// can circle for many seconds while newer generations queue up unseen.
    | Latest
    | Fixed of generation: int

type private Finish =
    | Died of Death
    | Filled

type private Replay =
    { Champion: Champion<Mind.Mind>
      Game: Game
      /// What the brain computed for the current position. It decides the next move.
      Thought: Mind.Thought }

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
let private brainSelect = element "brain" :?> HTMLSelectElement
let private option kind = element $"{(info kind).Value}-option"

let private show' (el: HTMLElement) visible =
    if visible then el.removeAttribute "hidden" else el.setAttribute ("hidden", "")

let private newSeed () = uint32 (JS.Math.random () * 4294967296.0)

let private worker = startWorker ()
let mutable private run = 0
let private champions = ResizeArray<Champion<Mind.Mind>>()
let mutable private training = Running
let mutable private nextPause = pauseEvery
let mutable private pick = Latest
let mutable private show = Waiting
let mutable private lastMove = 0.0
/// Generation under the mouse on the chart.
let mutable private hovered: int option = None
let mutable private circuits = kinds |> List.map (fun k -> k, Loading) |> Map.ofList
/// Fruit fly is the default. Its circuit loads after the page, so the first run starts once it arrives.
let mutable private species = WiredSpecies Fly

let private circuit () =
    match species with
    | WiredSpecies k ->
        match circuits[k] with
        | Ready(_, c) -> Some c
        | Loading
        | Unavailable -> None
    | NetSpecies
    | NeatSpecies
    | ProgramSpecies -> None

let private moveMs () = 160.0 / float speedSelect.value

/// A fresh random game, not the one the champion won in training. Its training game was a lucky draw,
/// so a fresh game shows how the brain really plays.
let private begin' (c: Champion<Mind.Mind>) =
    let g = Game.start (newSeed ())

    Playing
        { Champion = c
          Game = g
          Thought = Mind.think c.Brain (vision g) }

/// Hands the game in progress to another champion's brain.
let private swap (c: Champion<Mind.Mind>) (r: Replay) =
    { r with
        Champion = c
        Thought = Mind.think c.Brain (vision r.Game) }

let private nextChampion () =
    match pick with
    | Latest -> Seq.tryLast champions
    | Fixed gen when gen >= 1 && gen <= champions.Count -> Some champions[gen - 1]
    | Fixed _ -> None

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
            match step r.Thought.Turn r.Game with
            | Alive g -> Playing { r with Game = g; Thought = Mind.think r.Champion.Brain (vision g) }
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
        View.board boardCanvas r.Game r.Thought.Input
        View.thought networkCanvas r.Thought
        setText (element "generation") (string r.Champion.Generation)
        setText (element "score") (string r.Game.Score)
    | None -> ()

    setText
        (element "outcome")
        (match training, show with
         | Failed, _ -> "Training failed"
         | _, Ended(_, finish, _) -> finishLabel finish
         | _, Playing _ -> "Playing"
         | _, Waiting -> "Training")

    setText (element "trained") (string champions.Count)

    setText
        (element "latest")
        (match Seq.tryLast champions with
         | Some c -> string c.Score
         | None -> "–")

    setText
        (element "best")
        (if champions.Count = 0 then "–"
         else string (champions |> Seq.map (fun c -> c.Score) |> Seq.max))

    let current = replay |> Option.map (fun r -> r.Champion.Generation)
    View.chart chartCanvas (champions.ToArray()) current hovered
    chartCanvas.setAttribute ("aria-valuemax", string (max 1 champions.Count))

    match current with
    | Some g when g >= 1 && g <= champions.Count ->
        chartCanvas.setAttribute ("aria-valuenow", string g)
        chartCanvas.setAttribute ("aria-valuetext", $"Generation {g}, score {champions[g - 1].Score}")
    | _ -> ()

    setText
        trainButton
        (match training with
         | Running -> "Pause"
         | Paused
         | Failed -> "Resume")

let private send () =
    let order name (circuit: string option) : Order =
        {| Run = run
           Seed = newSeed ()
           Running = (training = Running)
           Species = name
           Circuit = circuit |}

    match species with
    | NetSpecies -> worker.postMessage (order "net" None)
    | NeatSpecies -> worker.postMessage (order "neat" None)
    | ProgramSpecies -> worker.postMessage (order "program" None)
    | WiredSpecies k ->
        match circuits[k] with
        | Ready(text, _) -> worker.postMessage (order "circuit" (Some text))
        // The circuit is still loading, and its arrival starts the run. Sending now would train the wrong species.
        | Loading
        | Unavailable -> ()

worker.onmessage <-
    fun e ->
        let r = e.data

        // Reports from a replaced population can still be in flight after Restart.
        if r.Run = run then
            match champion (circuit ()) r with
            | Some c ->
                champions.Add c

                match pick, show with
                | Latest, Playing replay -> show <- Playing(swap c replay)
                | _ -> ()
            | None -> console.error ("Dropped a report: circuit not loaded")

            if champions.Count >= nextPause && training = Running then
                training <- Paused
                nextPause <- champions.Count + pauseEvery
                send ()

worker.onerror <-
    fun error ->
        console.error ("Training worker failed", error)
        training <- Failed

let rec private frame (t: float) =
    advance t
    render ()
    window.requestAnimationFrame frame |> ignore

/// Plays a chosen generation now and stops following the latest.
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
            | Paused
            | Failed -> Running

        send ()

followBox.onchange <-
    fun _ ->
        if followBox.``checked`` then
            pick <- Latest

            match show, Seq.tryLast champions with
            | Playing r, Some c -> show <- Playing(swap c r)
            | _ -> ()
        else
            match show with
            | Playing r
            | Ended(r, _, _) -> pick <- Fixed r.Champion.Generation
            | Waiting -> ()

/// A new population of the current species. Reports from the old run are dropped by run number.
let private restart () =
    run <- run + 1
    champions.Clear()
    training <- Running
    nextPause <- pauseEvery
    pick <- Latest
    followBox.``checked`` <- true
    show <- Waiting
    send ()

restartButton.onclick <- fun _ -> restart ()

let private choose newSpecies =
    species <- newSpecies

    brainSelect.value <- (describe newSpecies).Value

    for s in allSpecies do
        show' (element (describe s).Legend) (s = newSpecies)

    restart ()

brainSelect.onchange <-
    fun _ ->
        choose (
            allSpecies
            |> List.tryFind (fun s -> (describe s).Value = brainSelect.value)
            |> Option.defaultValue NetSpecies
        )

/// Without its circuit a kind is disabled, and the page falls back to the network, so it still works.
let private unavailable kind (reason: string) =
    circuits <- circuits.Add(kind, Unavailable)
    (option kind).setAttribute ("disabled", "")
    (option kind).textContent <- $"{(info kind).Name} (unavailable)"
    console.error ($"{(info kind).Name} circuit unavailable: {reason}")

    if species = WiredSpecies kind then
        choose NetSpecies

for kind in kinds do
    (fetchText ((info kind).Url ()))
        .``then``(
            (fun text ->
                match parseCircuit text with
                | Ok c ->
                    circuits <- circuits.Add(kind, Ready(text, c))

                    if species = WiredSpecies kind then
                        restart ()
                | Error message -> unavailable kind message),
            (fun error -> unavailable kind (string error))
        )
    |> ignore

chartCanvas.onclick <-
    fun e ->
        View.generationAt chartCanvas champions.Count e.offsetX
        |> Option.iter select

chartCanvas.onmousemove <- fun e -> hovered <- View.generationAt chartCanvas champions.Count e.offsetX
chartCanvas.onmouseleave <- fun _ -> hovered <- None

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
