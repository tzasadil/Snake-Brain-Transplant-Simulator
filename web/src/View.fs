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
    let width = cell - 2.0 * inset

    // One thick line through the cell centers, so consecutive cells join and the body's path shows.
    // Neighbors that are not consecutive keep their gap. Square caps and miter joins fill whole cells.
    match g.Body with
    | first :: rest ->
        ctx.strokeStyle <- !^Color.snake
        ctx.lineWidth <- width
        ctx.lineCap <- "square"
        ctx.lineJoin <- "miter"
        ctx.beginPath ()
        ctx.moveTo (center first.X, center first.Y)

        for c in rest do
            ctx.lineTo (center c.X, center c.Y)

        ctx.stroke ()
        ctx.lineCap <- "butt"
        ctx.fillStyle <- !^Color.head
        ctx.fillRect (float first.X * cell + inset, float first.Y * cell + inset, width, width)
    | [] -> ()

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

let private testLabel =
    function
    | Program.Blocked ray -> $"blocked {rayLabels[ray]}"
    | Program.Within(ray, thing, cells) ->
        let name =
            match thing with
            | Program.Wall -> "wall"
            | Program.Body -> "body"
            | Program.Fruit -> "fruit"

        $"{name} {rayLabels[ray]} ≤{cells}"
    | Program.FruitTo side ->
        match side with
        | Program.Ahead -> "fruit ahead"
        | Program.Behind -> "fruit behind"
        | Program.Left -> "fruit left"
        | Program.Right -> "fruit right"
    | Program.Longer cells -> $"length > {cells}"

type private Line =
    { Guide: string
      Branch: string
      Text: string
      Lit: bool
      Leaf: bool }

/// One line per node, preorder, with tree guides. Lit lines lie on the path the answers took.
let private treeLines (tree: Program.Tree) (answers: bool list) =
    let rec go guide branch indent tree lit answers =
        match tree with
        | Program.Do turn ->
            [ { Guide = guide
                Branch = branch
                Text = outputNames[choiceIndex turn]
                Lit = lit
                Leaf = true } ]
        | Program.If(test, yes, no) ->
            let answer, rest =
                match answers with
                | a :: rest when lit -> Some a, rest
                | _ -> None, []

            { Guide = guide
              Branch = branch
              Text = testLabel test + "?"
              Lit = lit
              Leaf = false }
            :: go (indent + "├ ") "yes " (indent + "│ ") yes (answer = Some true) rest
            @ go (indent + "└ ") "no  " (indent + "  ") no (answer = Some false) rest

    go "" "" "" tree true answers

/// A program tree as indented rules. The tests on the way to this move are lit, the chosen move green.
let program (canvas: HTMLCanvasElement) (tree: Program.Tree) (answers: bool list) (choice: int) =
    let ctx, _, h = fit canvas
    let lines = treeLines tree answers
    let top = 30.0
    let lineH = min 22.0 ((h - top - 4.0) / float lines.Length)
    let font = $"{min 15.0 (lineH * 0.8)}px ui-monospace, monospace"
    ctx.font <- font
    ctx.textBaseline <- "middle"
    ctx.textAlign <- "left"
    let charW = ctx.measureText("m").width

    lines
    |> List.iteri (fun k line ->
        let y = top + lineH * (float k + 0.5)
        let x = 12.0 + charW * float line.Guide.Length
        ctx.globalAlpha <- if line.Lit then 1.0 else 0.45
        ctx.fillStyle <- !^Color.muted
        ctx.fillText (line.Guide + line.Branch, 12.0, y)

        ctx.fillStyle <-
            !^(if line.Lit && line.Leaf then Color.snake
               elif line.Lit then Color.text
               else Color.muted)

        ctx.fillText (line.Text, x + charW * float line.Branch.Length, y))

    let tests = lines |> List.filter (fun l -> not l.Leaf) |> List.length
    caption ctx [ "Turn", outputNames[choice]; "Tests", string tests ]

/// An arrow from x, y along the unit direction dx, dy. Wing sets the head size.
let private arrow (ctx: CanvasRenderingContext2D) (color: string) width wing x y dx dy length =
    let tipX, tipY = x + dx * length, y + dy * length
    ctx.strokeStyle <- !^color
    ctx.fillStyle <- !^color
    ctx.lineWidth <- width
    ctx.lineCap <- "round"
    line ctx x y (tipX - dx * wing) (tipY - dy * wing)
    ctx.beginPath ()
    ctx.moveTo (tipX, tipY)
    ctx.lineTo (tipX - dx * wing * 1.6 - dy * wing, tipY - dy * wing * 1.6 + dx * wing)
    ctx.lineTo (tipX - dx * wing * 1.6 + dy * wing, tipY - dy * wing * 1.6 - dx * wing)
    ctx.closePath ()
    ctx.fill ()
    ctx.lineCap <- "butt"

/// The Hamiltonian cycle on the board, each cell numbered by its position along it, with the snake's body on top
/// and an arrow to the cell it enters next. An earlier version also lit the stretch a shortcut may reach,
/// but that needed explaining.
let hamilton (canvas: HTMLCanvasElement) (c: Hamilton.Cycle) (plan: Hamilton.Plan) (choice: int) =
    let ctx, w, h = fit canvas
    // Clears the brain picker, which overlaps the canvas top right.
    let top = 52.0
    let side = min (w - 24.0) (h - top - 8.0)
    let cell = side / float size
    let ox = (w - side) / 2.0
    let n = c.Cells.Length
    let at (i: int) = ox + (float (i % size) + 0.5) * cell, top + (float (i / size) + 0.5) * cell
    let center k = at c.Cells[k % n]
    let centerOf (p: Cell) = at (p.Y * size + p.X)

    ctx.lineJoin <- "round"
    ctx.lineCap <- "round"
    ctx.strokeStyle <- !^Color.muted
    ctx.globalAlpha <- 0.35
    ctx.lineWidth <- 2.0
    ctx.beginPath ()
    let x0, y0 = center 0
    ctx.moveTo (x0, y0)

    for k in 1..n do
        let x, y = center k
        ctx.lineTo (x, y)

    ctx.stroke ()
    ctx.globalAlpha <- 1.0

    ctx.font <- $"{min 11.0 (cell * 0.26)}px system-ui, sans-serif"
    ctx.textAlign <- "left"
    ctx.textBaseline <- "top"
    ctx.fillStyle <- !^Color.muted

    for k in 0 .. n - 1 do
        let i = c.Cells[k]
        ctx.fillText (string k, ox + float (i % size) * cell + 3.0, top + float (i / size) * cell + 2.0)

    let fx, fy = center plan.Fruit
    ctx.fillStyle <- !^Color.fruit
    circle ctx fx fy (cell * 0.18)
    ctx.fill ()

    ctx.strokeStyle <- !^Color.snake
    ctx.lineWidth <- cell * 0.16
    plan.Body |> List.pairwise |> List.iter (fun (a, b) ->
        let ax, ay = centerOf a
        let bx, by = centerOf b
        line ctx ax ay bx by)

    let hx, hy = center plan.Head
    ctx.fillStyle <- !^Color.snake
    circle ctx hx hy (cell * 0.14)
    ctx.fill ()

    // Its tip stops short of the next cell's center, so the number stays readable.
    let tx, ty = center plan.Target
    ctx.globalAlpha <- 1.0
    arrow ctx Color.head 2.5 (cell * 0.16) hx hy ((tx - hx) / cell) ((ty - hy) / cell) (cell * 0.75)

    caption ctx [ "Turn", outputNames[choice]; "Jump", string ((plan.Target - plan.Head + n) % n) ]

/// A Q-table as a policy map: in every free cell, the move the table picks if the head were there, facing the
/// way it faces now. Brightness is the move's value. Beside it, the current state and its three values.
let qtable (canvas: HTMLCanvasElement) (q: QTable.Table) (g: Game) (s: int) (choice: int) =
    let ctx, w, h = fit canvas
    // Clears the brain picker, which overlaps the canvas top right.
    let top = 52.0
    let side = min (h - top - 8.0) (w * 0.6)
    let cell = side / float size
    let ox = 8.0
    let center x y = ox + (float x + 0.5) * cell, top + (float y + 0.5) * cell
    let peak = peak q
    let moveRay = [| 6; 0; 2 |]

    ctx.strokeStyle <- !^Color.grid
    ctx.lineWidth <- 1.0
    ctx.strokeRect (ox, top, side, side)

    for x in 0 .. size - 1 do
        for y in 0 .. size - 1 do
            let cx, cy = center x y
            let here = { X = x; Y = y }

            if g.Occupied[y * size + x] then
                ctx.globalAlpha <- 0.5
                ctx.fillStyle <- !^Color.snake
                ctx.fillRect (cx - cell * 0.35, cy - cell * 0.35, cell * 0.7, cell * 0.7)
            elif x = g.Fruit.X && y = g.Fruit.Y then
                ctx.globalAlpha <- 1.0
                ctx.fillStyle <- !^Color.fruit
                circle ctx cx cy (cell * 0.22)
                ctx.fill ()
            else
                let k = QTable.stateAt g here
                let a = QTable.turnIndex (QTable.greedy q k)
                let value = q[k * 3 + a]
                let dx, dy = rayDirection g moveRay[a]
                ctx.globalAlpha <- 0.2 + 0.8 * abs value / peak
                let color = if value >= 0.0 then Color.positive else Color.negative
                arrow ctx color 1.5 (cell * 0.1) (cx - float dx * cell * 0.25) (cy - float dy * cell * 0.25) (float dx) (float dy) (cell * 0.5)

    let head = List.head g.Body
    let hx, hy = center head.X head.Y
    let dx, dy = rayDirection g moveRay[choice]
    ctx.globalAlpha <- 1.0
    ctx.fillStyle <- !^Color.head
    ctx.fillRect (hx - cell * 0.35, hy - cell * 0.35, cell * 0.7, cell * 0.7)
    arrow ctx Color.head 2.5 (cell * 0.16) hx hy (float dx) (float dy) (cell * 0.75)

    // The current state, as the table sees it: danger on each side, and where the fruit is.
    let x0 = ox + side + 24.0
    let danger = s / 9
    let ahead, right = (s % 9) / 3 - 1, s % 3 - 1
    let fruitRay = Array.findIndex ((=) (ahead, right)) [| 1, 0; 1, 1; 0, 1; -1, 1; -1, 0; -1, -1; 0, -1; 1, -1 |]
    ctx.font <- "13px system-ui, sans-serif"
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    ctx.fillStyle <- !^Color.muted
    ctx.fillText ("Danger", x0, top + 10.0)
    ctx.fillText ("Fruit", x0, top + 34.0)

    [| "←"; "↑"; "→" |]
    |> Array.iteri (fun i label ->
        ctx.fillStyle <- !^(if danger &&& (4 >>> i) <> 0 then Color.fruit else Color.muted)
        ctx.fillText (label, x0 + 56.0 + float i * 18.0, top + 10.0))

    ctx.fillStyle <- !^Color.fruit
    ctx.fillText (rayLabels[fruitRay], x0 + 56.0, top + 34.0)

    let barW = w - x0 - 8.0

    outputNames
    |> Array.iteri (fun a label ->
        let value = q[s * 3 + a]
        let y = top + 70.0 + float a * 42.0
        ctx.fillStyle <- !^(if a = choice then Color.text else Color.muted)
        ctx.fillText ($"{label} {value:F2}", x0, y)
        ctx.fillStyle <- !^(if value >= 0.0 then Color.positive else Color.negative)
        ctx.globalAlpha <- if a = choice then 1.0 else 0.5
        ctx.fillRect (x0, y + 10.0, max 1.0 (barW * abs value / peak), 6.0)
        ctx.globalAlpha <- 1.0)

    let learned =
        Array.init QTable.stateCount id
        |> Array.filter (fun k -> q[k * 3] <> 0.0 || q[k * 3 + 1] <> 0.0 || q[k * 3 + 2] <> 0.0)
        |> Array.length

    caption ctx [ "Turn", outputNames[choice]; "States seen", $"{learned}/{QTable.reachable}" ]

/// Moves the spike raster keeps on screen.
let private rasterMoves = 10

/// Recent rasters of the game on the board with their move numbers, oldest first. Module state, because only this
/// view needs a history.
let private rasterHistory = ResizeArray<int * Spiking.Raster>()

/// A scrolling spike raster: one row per neuron, time to the right, the latest move at the right edge.
/// Moves on top, hidden neurons in the middle, senses at the bottom.
let spiking (canvas: HTMLCanvasElement) (raster: Spiking.Raster) (move: int) (choice: int) =
    // A later move scrolls. The same move again means a new champion took over the position, so it replaces the
    // last column. An earlier move means a new game.
    match Seq.tryLast rasterHistory with
    | Some(last, _) when move = last -> rasterHistory[rasterHistory.Count - 1] <- (move, raster)
    | Some(last, _) when move > last -> rasterHistory.Add((move, raster))
    | _ ->
        rasterHistory.Clear()
        rasterHistory.Add((move, raster))

    if rasterHistory.Count > rasterMoves then
        rasterHistory.RemoveAt 0

    let ctx, w, h = fit canvas
    // Clears the brain picker, which overlaps the canvas top right.
    let top = 52.0
    let left = 72.0
    let gap = 10.0
    let outputH = 16.0
    let rowH = (h - top - 8.0 - 3.0 * outputH - 2.0 * gap) / float Spiking.sources
    let colW = (w - left - 8.0) / float (rasterMoves * Spiking.steps)
    let hiddenTop = top + 3.0 * outputH + gap
    let inputTop = hiddenTop + float Spiking.hidden * rowH + gap

    // Row top of neuron n: inputs come first in a raster, but sit at the bottom of the picture.
    let rowY n =
        if n < Spiking.inputs then inputTop + float n * rowH
        elif n < Spiking.sources then hiddenTop + float (n - Spiking.inputs) * rowH
        else top + float (n - Spiking.sources) * outputH

    let rowHeight n = if n >= Spiking.sources then outputH else rowH
    let shown = rasterHistory.Count

    for m in 0 .. shown - 1 do
        let _, r = rasterHistory[m]
        let x0 = left + float (rasterMoves - shown + m) * float Spiking.steps * colW

        ctx.globalAlpha <- 1.0
        ctx.strokeStyle <- !^Color.grid
        ctx.lineWidth <- 1.0
        line ctx x0 top x0 (h - 8.0)

        for n in 0 .. Spiking.neurons - 1 do
            ctx.fillStyle <-
                !^(if n < Spiking.inputs then Color.input
                   elif n < Spiking.sources then Color.text
                   else Color.snake)

            ctx.globalAlpha <- if m = shown - 1 then 1.0 else 0.6

            for t in 0 .. Spiking.steps - 1 do
                if r.Spikes[n * Spiking.steps + t] then
                    ctx.fillRect (x0 + float t * colW, rowY n, max 1.0 (colW - 0.5), max 1.0 (rowHeight n * 0.8))

    ctx.globalAlpha <- 1.0
    ctx.font <- "12px system-ui, sans-serif"
    ctx.textAlign <- "right"
    ctx.textBaseline <- "middle"

    outputNames
    |> Array.iteri (fun o label ->
        ctx.fillStyle <- !^(if o = choice then Color.text else Color.muted)
        ctx.fillText (label, left - 8.0, top + (float o + 0.5) * outputH))

    ctx.fillStyle <- !^Color.muted
    ctx.fillText ("Hidden", left - 8.0, hiddenTop + float Spiking.hidden * rowH / 2.0)
    ctx.fillText ("Senses", left - 8.0, inputTop + float Spiking.inputs * rowH / 2.0)
    let spikes = raster.Spikes |> Array.filter id |> Array.length
    caption ctx [ "Turn", outputNames[choice]; "Spikes", string spikes ]

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
    | Mind.Path(tree, answers) -> program canvas tree answers (choiceIndex t.Turn)
    | Mind.Route(cycle, plan) -> hamilton canvas cycle plan (choiceIndex t.Turn)
    | Mind.Policy(table, game, state) -> qtable canvas table game state (choiceIndex t.Turn)
    | Mind.Spikes(_, raster, move) -> spiking canvas raster move (choiceIndex t.Turn)

/// Generation x position on the chart. Bars keep a minimum width until history outgrows it.
let private slots count = max count 60

/// Best score per generation, unlabeled. The generation on the board is highlighted, the hovered one brightened and labeled.
/// Unit names a bar, such as "Gen", or "Game" for brains that learn during play.
let chart (canvas: HTMLCanvasElement) (champions: Champion<_>[]) (unit: string) (current: int option) (hovered: int option) =
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
        ctx.fillText ($"{unit} {g} · score {champions[g - 1].Score}", w, 2.0)
    | None -> ()

/// Generation under a click at x, if any.
let generationAt (canvas: HTMLCanvasElement) (count: int) (x: float) =
    let slot = int (x / canvas.clientWidth * float (slots count))
    if slot >= 0 && slot < count then Some(slot + 1) else None
