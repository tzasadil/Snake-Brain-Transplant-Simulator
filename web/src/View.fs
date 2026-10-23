/// Canvas drawing: board, network, and score history.
module SnakeEvo.View

open Browser.Types
open Fable.Core.JsInterop
open SnakeEvo.Game
open SnakeEvo.Evolution

/// Mirrors the CSS custom properties in style.css. Canvas cannot read them cheaply each frame.
module Color =
    let panel = "#151821"
    let grid = "#262b38"
    let text = "#e6e8ee"
    let muted = "#8a91a3"
    let wall = "#94a3b8"
    let snake = "#4ade80"
    let head = "#bbf7d0"
    let fruit = "#f87171"
    let positive = "#60a5fa"
    let negative = "#fb923c"
    let input = "#facc15"
    let gap = "#c084fc"

/// Ray labels in vision order, as seen by the snake with forward = up.
let private rayLabels = [| "↑"; "↗"; "→"; "↘"; "↓"; "↙"; "←"; "↖" |]
let private senseColors = [| Color.wall; Color.snake; Color.fruit |]

/// Inputs after the 24 ray inputs: fruit compass ahead and right, then snake length. See Game.vision.
let private extraLabels = [| "fruit ↑"; "fruit →"; "length" |]
let private extraColors = [| Color.fruit; Color.fruit; Color.snake |]
let private outputLabels = [| "Left"; "Straight"; "Right" |]

/// Matches the canvas buffer to its CSS size and pixel ratio, so drawing stays sharp.
/// Returns the context scaled to CSS pixels, with the CSS width and height.
let fit (canvas: HTMLCanvasElement) =
    let ratio = Browser.Dom.window.devicePixelRatio
    let w = canvas.clientWidth
    let h = canvas.clientHeight
    let bufferW = int (w * ratio)
    let bufferH = int (h * ratio)

    if canvas.width <> float bufferW || canvas.height <> float bufferH then
        canvas.width <- float bufferW
        canvas.height <- float bufferH

    let ctx = canvas.getContext_2d ()
    ctx.setTransform (ratio, 0.0, 0.0, ratio, 0.0, 0.0)
    ctx.clearRect (0.0, 0.0, w, h)
    ctx, w, h

let private circle (ctx: CanvasRenderingContext2D) x y r =
    ctx.beginPath ()
    ctx.arc (x, y, r, 0.0, 2.0 * System.Math.PI)

let private line (ctx: CanvasRenderingContext2D) x1 y1 x2 y2 =
    ctx.beginPath ()
    ctx.moveTo (x1, y1)
    ctx.lineTo (x2, y2)
    ctx.stroke ()

/// The board, plus the 8 vision rays from the head. A ray takes the color of what it sees first: fruit, then body.
let board (canvas: HTMLCanvasElement) (g: Game) (seen: float[]) =
    let ctx, w, _ = fit canvas
    let cell = w / float size
    let center v = (float v + 0.5) * cell

    ctx.fillStyle <- !^Color.panel
    ctx.fillRect (0.0, 0.0, w, w)
    ctx.fillStyle <- !^Color.grid

    for x in 0 .. size - 1 do
        for y in 0 .. size - 1 do
            circle ctx (center x) (center y) (cell * 0.06)
            ctx.fill ()

    let head = List.head g.Body
    ctx.lineWidth <- 2.0

    for i in 0..7 do
        let dx, dy = rayDirection g i
        // Wall input is 1 / distance to the first cell outside the board.
        let reach = 1.0 / seen[i * 3] - 0.5

        let color, alpha =
            if seen[i * 3 + 2] > 0.0 then Color.fruit, 0.7
            elif seen[i * 3 + 1] > 0.0 then Color.snake, 0.45
            else Color.wall, 0.18

        ctx.strokeStyle <- !^color
        ctx.globalAlpha <- alpha
        line ctx (center head.X) (center head.Y) (center head.X + float dx * reach * cell) (center head.Y + float dy * reach * cell)

    ctx.globalAlpha <- 1.0
    ctx.fillStyle <- !^Color.fruit
    circle ctx (center g.Fruit.X) (center g.Fruit.Y) (cell * 0.32)
    ctx.fill ()

    let inset = cell * 0.08

    g.Body
    |> List.iteri (fun i c ->
        ctx.fillStyle <- !^(if i = 0 then Color.head else Color.snake)
        ctx.fillRect (float c.X * cell + inset, float c.Y * cell + inset, cell - 2.0 * inset, cell - 2.0 * inset))

/// The network. Edge color = weight sign. Edge strength = the signal it carries right now,
/// so the picture shows which senses drive the current decision.
let network (canvas: HTMLCanvasElement) (brain: Brain.Brain) (acts: float[][]) (choice: int) =
    let ctx, w, h = fit canvas
    let top = 14.0
    let usable = h - 2.0 * top
    let columns = [| 70.0; w * 0.5; w - 78.0 |]
    let inputs = acts[0].Length
    let gaps = (inputs - 1) / 3
    let inputStep = usable / (float inputs + 0.8 * float gaps)

    // Inputs come in groups of 3, with a gap between groups: 8 rays, then compass and length.
    let inputY i =
        top + inputStep * (float i + 0.8 * float (i / 3)) + inputStep * 0.5

    let evenY n i =
        let step = usable / float n
        top + step * (float i + 0.5)

    let y layer i =
        if layer = 0 then inputY i else evenY acts[layer].Length i

    let peak (values: float[]) =
        values |> Array.fold (fun m v -> max m (abs v)) 1e-9

    for l in 0 .. brain.Length - 1 do
        let strength = peak acts[l]

        brain[l]
        |> Array.iteri (fun j weights ->
            for i in 0 .. acts[l].Length - 1 do
                let signal = abs (acts[l][i] / strength * weights[i])
                ctx.strokeStyle <- !^(if weights[i] >= 0.0 then Color.positive else Color.negative)
                ctx.globalAlpha <- 0.04 + 0.8 * min 1.0 signal
                ctx.lineWidth <- 0.5 + 1.5 * min 1.0 signal
                line ctx columns[l] (y l i) columns[l + 1] (y (l + 1) j))

    ctx.globalAlpha <- 1.0
    let radius = min 6.0 (inputStep * 0.42 |> max 2.5)

    for l in 0 .. acts.Length - 1 do
        let strength = peak acts[l]

        acts[l]
        |> Array.iteri (fun i v ->
            let color =
                if l = 0 && i < 24 then senseColors[i % 3]
                elif l = 0 then extraColors[i - 24]
                elif l = acts.Length - 1 && i = choice then Color.snake
                else Color.text

            let level =
                if l = acts.Length - 1 then (if i = choice then 1.0 else 0.15)
                else abs v / strength

            circle ctx columns[l] (y l i) radius
            ctx.fillStyle <- !^Color.panel
            ctx.fill ()
            ctx.globalAlpha <- 0.12 + 0.88 * level
            ctx.fillStyle <- !^color
            ctx.fill ()
            ctx.globalAlpha <- 1.0)

    ctx.font <- "13px system-ui, sans-serif"
    ctx.textBaseline <- "middle"
    ctx.textAlign <- "right"
    ctx.fillStyle <- !^Color.muted

    rayLabels |> Array.iteri (fun r label -> ctx.fillText (label, columns[0] - 14.0, inputY (r * 3 + 1)))
    ctx.font <- "12px system-ui, sans-serif"
    extraLabels |> Array.iteri (fun i label -> ctx.fillText (label, columns[0] - 12.0, inputY (24 + i)))
    ctx.font <- "13px system-ui, sans-serif"

    ctx.textAlign <- "left"

    outputLabels
    |> Array.iteri (fun i label ->
        ctx.fillStyle <- !^(if i = choice then Color.text else Color.muted)
        ctx.fillText (label, columns[2] + 14.0, evenY 3 i))

/// A connectome circuit at real soma positions over a faint outline: the fly brain seen from behind,
/// so its left is on the left, or the worm from the side, head on the left.
/// Synapses show only while they carry signal, so the active paths stand out of thousands.
let connectome (canvas: HTMLCanvasElement) (c: Connectome.Circuit) (h: float[]) (turn: Turn) =
    let ctx, w, hgt = fit canvas
    let top = 30.0
    let size = min (w - 24.0) (hgt - top - 12.0)
    let ox = (w - size) / 2.0
    // Positions span 0 to 1 across, but less in height, so the drawing centers what is there.
    let ys = Array.append (c.Neurons |> Array.map (fun n -> n.Y)) (Array.init (c.Cloud.Length / 2) (fun k -> c.Cloud[2 * k + 1]))
    let low, high = Array.min ys, Array.max ys
    let oy = top + (hgt - top - 12.0 - (high - low) * size) / 2.0 - low * size
    let px x = ox + x * size
    let py y = oy + y * size

    ctx.fillStyle <- !^Color.grid

    for k in 0 .. c.Cloud.Length / 2 - 1 do
        ctx.fillRect (px c.Cloud[2 * k] - 1.0, py c.Cloud[2 * k + 1] - 1.0, 2.0, 2.0)

    let peak = h |> Array.fold max 1e-9

    for j in 0 .. c.Neurons.Length - 1 do
        for e in c.Start[j] .. c.Start[j + 1] - 1 do
            let signal = abs (c.Weight[e] * h[c.Pre[e]]) / peak * 4.0

            if signal > 0.05 then
                let pre = c.Neurons[c.Pre[e]]
                ctx.strokeStyle <- !^(if c.Weight[e] >= 0.0 then Color.positive else Color.negative)
                ctx.globalAlpha <- min 0.8 signal
                ctx.lineWidth <- 0.5 + min 1.5 signal
                line ctx (px pre.X) (py pre.Y) (px c.Neurons[j].X) (py c.Neurons[j].Y)

        for e in c.GapStart[j] .. c.GapStart[j + 1] - 1 do
            let peer = c.GapPeer[e]
            let signal = abs (c.GapWeight[e] * h[peer]) / peak * 4.0

            if signal > 0.05 then
                ctx.strokeStyle <- !^Color.gap
                ctx.globalAlpha <- min 0.8 signal
                ctx.lineWidth <- 0.5 + min 1.5 signal
                line ctx (px c.Neurons[peer].X) (py c.Neurons[peer].Y) (px c.Neurons[j].X) (py c.Neurons[j].Y)

    let steering =
        match turn with
        | TurnLeft -> c.Left |> Array.map fst
        | TurnRight -> c.Right |> Array.map fst
        | Straight -> [||]

    c.Neurons
    |> Array.iteri (fun i n ->
        let color, radius =
            match n.Role with
            | Connectome.Input -> Color.input, 3.5
            | Connectome.Inter -> Color.text, 2.5
            | Connectome.Output -> Color.snake, 5.0

        circle ctx (px n.X) (py n.Y) radius
        ctx.globalAlpha <- 1.0
        ctx.fillStyle <- !^Color.panel
        ctx.fill ()
        ctx.globalAlpha <- 0.15 + 0.85 * (h[i] / peak)
        ctx.fillStyle <- !^color
        ctx.fill ()

        if Array.contains i steering then
            ctx.globalAlpha <- 1.0
            ctx.strokeStyle <- !^Color.snake
            ctx.lineWidth <- 2.0
            circle ctx (px n.X) (py n.Y) (radius + 3.0)
            ctx.stroke ())

    ctx.globalAlpha <- 1.0
    ctx.font <- "13px system-ui, sans-serif"
    ctx.textAlign <- "left"
    ctx.textBaseline <- "top"
    ctx.fillStyle <- !^Color.muted
    ctx.fillText ("Turn", 0.0, 4.0)
    ctx.fillStyle <- !^Color.text

    ctx.fillText (
        (match turn with
         | TurnLeft -> "Left"
         | Straight -> "Straight"
         | TurnRight -> "Right"),
        36.0,
        4.0
    )

let private choiceIndex =
    function
    | TurnLeft -> 0
    | Straight -> 1
    | TurnRight -> 2

/// Draws whichever kind of brain produced the thought.
let thought (canvas: HTMLCanvasElement) (t: Mind.Thought) =
    match t.Activity with
    | Mind.Layers(brain, layers) -> network canvas brain layers (choiceIndex t.Turn)
    | Mind.Neurons(circuit, h) -> connectome canvas circuit h t.Turn

/// Generation x position on the chart. Bars keep a minimum width until history outgrows it.
let private slots count = max count 60

/// Best score per generation, unlabeled. The replayed generation is highlighted, the hovered one brightened and labeled.
let chart (canvas: HTMLCanvasElement) (champions: Champion<_>[]) (current: int option) (hovered: int option) =
    let ctx, w, h = fit canvas
    let plotH = h - 26.0
    let top = champions |> Array.fold (fun m c -> max m c.Score) 0
    // Floor of 5 keeps early, tiny scores from filling the whole height.
    let best = max top 5
    let barW = w / float (slots champions.Length)

    champions
    |> Array.iteri (fun i c ->
        let barH = max 1.0 (float c.Score / float best * plotH)
        let isCurrent = current = Some c.Generation
        ctx.fillStyle <- !^(if isCurrent then Color.head else Color.snake)

        ctx.globalAlpha <-
            if isCurrent || hovered = Some c.Generation then 1.0
            else 0.45

        ctx.fillRect (float i * barW, h - 4.0 - barH, max 1.0 (barW - 1.0), barH))

    ctx.globalAlpha <- 1.0

    // The top strip stays free for this label, so it never covers a bar.
    match hovered |> Option.filter (fun g -> g >= 1 && g <= champions.Length) with
    | Some g ->
        ctx.font <- "12px system-ui, sans-serif"
        ctx.textBaseline <- "top"
        ctx.textAlign <- "right"
        ctx.fillStyle <- !^Color.text
        ctx.fillText ($"Gen {g} · score {champions[g - 1].Score}", w, 2.0)
    | None -> ()

/// Generation under a click at x, if any.
let generationAt (canvas: HTMLCanvasElement) (count: int) (x: float) =
    let slot = int (x / canvas.clientWidth * float (slots count))
    if slot >= 0 && slot < count then Some(slot + 1) else None
