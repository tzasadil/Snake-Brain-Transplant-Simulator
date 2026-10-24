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

let private choiceIndex =
    function
    | TurnLeft -> 0
    | Straight -> 1
    | TurnRight -> 2
let private outputNames = [| "Left"; "Straight"; "Right" |]

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

/// Input y positions: groups of 3 with a gap between groups. 8 rays, then compass and length, then NEAT's bias.
let private inputLayout top usable count =
    let gaps = (count - 1) / 3
    let step = usable / (float count + 0.8 * float gaps)
    step, (fun i -> top + step * (float i + 0.8 * float (i / 3)) + step * 0.5)

let private inputColor i =
    if i < 24 then senseColors[i % 3] else extraColors[i - 24]

let private inputLabels (ctx: CanvasRenderingContext2D) x (inputY: int -> float) =
    ctx.font <- "13px system-ui, sans-serif"
    ctx.textBaseline <- "middle"
    ctx.textAlign <- "right"
    ctx.fillStyle <- !^Color.muted
    rayLabels |> Array.iteri (fun r label -> ctx.fillText (label, x - 14.0, inputY (r * 3 + 1)))
    ctx.font <- "12px system-ui, sans-serif"
    extraLabels |> Array.iteri (fun i label -> ctx.fillText (label, x - 12.0, inputY (24 + i)))
    ctx.font <- "13px system-ui, sans-serif"

let private outputLabels (ctx: CanvasRenderingContext2D) x (outputY: int -> float) choice =
    ctx.textAlign <- "left"

    outputNames
    |> Array.iteri (fun i label ->
        ctx.fillStyle <- !^(if i = choice then Color.text else Color.muted)
        ctx.fillText (label, x + 14.0, outputY i))

/// A neuron lit by its level, 0 to 1, over a panel-colored disc so links behind it stay hidden.
let private dot (ctx: CanvasRenderingContext2D) x y radius (color: string) level =
    circle ctx x y radius
    ctx.fillStyle <- !^Color.panel
    ctx.fill ()
    ctx.globalAlpha <- 0.12 + 0.88 * level
    ctx.fillStyle <- !^color
    ctx.fill ()
    ctx.globalAlpha <- 1.0

/// A weighted link. Color = weight sign, strength = the signal it carries right now, 0 to 1.
let private wire (ctx: CanvasRenderingContext2D) weight signal x1 y1 x2 y2 =
    ctx.strokeStyle <- !^(if weight >= 0.0 then Color.positive else Color.negative)
    ctx.globalAlpha <- 0.04 + 0.8 * min 1.0 signal
    ctx.lineWidth <- 0.5 + 1.5 * min 1.0 signal
    line ctx x1 y1 x2 y2

/// Label and value pairs along the top left, like "Turn Left". The top right holds the brain picker.
let private caption (ctx: CanvasRenderingContext2D) (items: (string * string) list) =
    ctx.globalAlpha <- 1.0
    ctx.font <- "13px system-ui, sans-serif"
    ctx.textAlign <- "left"
    ctx.textBaseline <- "top"
    let mutable x = 0.0

    for label, value in items do
        ctx.fillStyle <- !^Color.muted
        ctx.fillText (label, x, 4.0)
        x <- x + ctx.measureText(label).width + 6.0
        ctx.fillStyle <- !^Color.text
        ctx.fillText (value, x, 4.0)
        x <- x + ctx.measureText(value).width + 16.0

let private peak (values: float[]) =
    values |> Array.fold (fun m v -> max m (abs v)) 1e-9

/// The network. Edge color = weight sign. Edge strength = the signal it carries right now,
/// so the picture shows which senses drive the current decision.
let network (canvas: HTMLCanvasElement) (brain: Brain.Brain) (acts: float[][]) (choice: int) =
    let ctx, w, h = fit canvas
    let top = 14.0
    let usable = h - 2.0 * top
    let columns = [| 70.0; w * 0.5; w - 78.0 |]
    let inputStep, inputY = inputLayout top usable acts[0].Length

    let evenY n i =
        let step = usable / float n
        top + step * (float i + 0.5)

    let y layer i =
        if layer = 0 then inputY i else evenY acts[layer].Length i

    for l in 0 .. brain.Length - 1 do
        let strength = peak acts[l]

        brain[l]
        |> Array.iteri (fun j weights ->
            for i in 0 .. acts[l].Length - 1 do
                let signal = abs (acts[l][i] / strength * weights[i])
                wire ctx weights[i] signal columns[l] (y l i) columns[l + 1] (y (l + 1) j))

    ctx.globalAlpha <- 1.0
    let radius = min 6.0 (inputStep * 0.42 |> max 2.5)

    for l in 0 .. acts.Length - 1 do
        let strength = peak acts[l]

        acts[l]
        |> Array.iteri (fun i v ->
            let color =
                if l = 0 then inputColor i
                elif l = acts.Length - 1 && i = choice then Color.snake
                else Color.text

            let level =
                if l = acts.Length - 1 then (if i = choice then 1.0 else 0.15)
                else abs v / strength

            dot ctx columns[l] (y l i) radius color level)

    inputLabels ctx columns[0] inputY
    outputLabels ctx columns[2] (evenY 3) choice

/// A NEAT net: inputs left, outputs right, grown neurons in between, one column per depth.
/// Disabled links are left out.
let neat (canvas: HTMLCanvasElement) (net: Neat.Net) (value: float[]) (choice: int) =
    let ctx, w, h = fit canvas
    // Below the caption. NEAT has one more input row than the network, the bias.
    let top = 24.0
    let usable = h - top - 4.0
    let left, right = 70.0, w - 78.0
    let n = net.Ids.Length
    let hidden = n - Neat.firstHidden
    let inputStep, inputY = inputLayout top usable (Neat.bias + 1)
    let deepest = net.Depth[n - 1]
    let x i = left + (right - left) * float net.Depth[i] / float deepest

    let evenY count k =
        let step = usable / float count
        top + step * (float k + 0.5)

    // Index order is depth, then id, so each column's nodes are consecutive.
    let y =
        Array.init n (fun i ->
            if i <= Neat.bias then
                inputY i
            elif i >= n - Neat.outputCount then
                evenY Neat.outputCount (i - (n - Neat.outputCount))
            else
                let column = [| Neat.bias + 1 .. n - Neat.outputCount - 1 |] |> Array.filter (fun k -> net.Depth[k] = net.Depth[i])
                evenY column.Length (Array.findIndex ((=) i) column))

    let strength =
        Array.init net.Source.Length (fun e -> value[net.Source[e]] * net.Weight[e]) |> peak

    for j in 0 .. n - 1 do
        for e in net.Start[j] .. net.Start[j + 1] - 1 do
            let i = net.Source[e]
            wire ctx net.Weight[e] (abs (value[i] * net.Weight[e]) / strength) (x i) y[i] (x j) y[j]

    ctx.globalAlpha <- 1.0
    let radius = min 6.0 (inputStep * 0.42 |> max 2.5)
    let level = peak value[.. n - Neat.outputCount - 1]

    for i in 0 .. n - 1 do
        if i < Neat.bias then
            dot ctx (x i) y[i] radius (inputColor i) (abs value[i] / level)
        elif i = Neat.bias then
            dot ctx (x i) y[i] radius Color.muted 1.0
        elif i < n - Neat.outputCount then
            dot ctx (x i) y[i] radius Color.text (abs value[i] / level)
        else
            let k = i - (n - Neat.outputCount)
            dot ctx (x i) y[i] radius (if k = choice then Color.snake else Color.text) (if k = choice then 1.0 else 0.15)

    inputLabels ctx left inputY
    ctx.font <- "12px system-ui, sans-serif"
    ctx.fillText ("bias", left - 12.0, inputY Neat.bias)
    ctx.font <- "13px system-ui, sans-serif"
    outputLabels ctx right (evenY Neat.outputCount) choice
    caption ctx [ "Grown", string hidden; "Links", string net.Source.Length ]

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

    caption ctx [ "Turn", outputNames[choiceIndex turn] ]

/// Draws whichever kind of brain produced the thought.
let thought (canvas: HTMLCanvasElement) (t: Mind.Thought) =
    match t.Activity with
    | Mind.Layers(brain, layers) -> network canvas brain layers (choiceIndex t.Turn)
    | Mind.Neurons(circuit, h) -> connectome canvas circuit h t.Turn
    | Mind.Graph(net, value) -> neat canvas net value (choiceIndex t.Turn)

/// Generation x position on the chart. Bars keep a minimum width until history outgrows it.
let private slots count = max count 60

/// Best score per generation, unlabeled. The generation on the board is highlighted, the hovered one brightened and labeled.
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
