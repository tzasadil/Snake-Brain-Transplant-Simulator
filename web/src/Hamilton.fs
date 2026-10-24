/// A Hamiltonian cycle: a closed path through every cell exactly once. A snake that follows it can never hit itself,
/// and shortcuts toward the fruit keep that safety as long as they never pass the tail. Nothing here learns:
/// evolution only keeps whichever random cycle scored best.
module SnakeEvo.Hamilton

open SnakeEvo.Rng
open SnakeEvo.Game

type Cycle =
    { /// Cell, y * size + x, at each position along the cycle. Positions count up from 0.
      Cells: int[]
      /// Position of each cell. The inverse of Cells.
      Order: int[] }

let private cellCount = size * size

let private adjacent a b =
    abs (a % size - b % size) + abs (a / size - b / size) = 1

/// Checks that the cells form a cycle through the whole board. The worker built them, so failing is a bug.
let ofCells (cells: int[]) =
    let order = Array.create cellCount -1

    if cells.Length <> cellCount then
        failwith "cycle must cover every cell"

    cells
    |> Array.iteri (fun k cell ->
        if cell < 0 || cell >= cellCount || order[cell] >= 0 then
            failwith "cycle must visit each cell once"

        order[cell] <- k

        if not (adjacent cell cells[(k + 1) % cellCount]) then
            failwith "cycle steps must join neighboring cells")

    { Cells = cells; Order = order }

/// A random cycle from a random spanning tree over 2x2 blocks. Each block starts as its own small loop.
/// Each tree edge joins two neighboring loops into one: it removes the two facing sides and adds two links
/// across. A tree has no loops, so the result is one cycle through every cell. Needs an even board size.
let random (rng: Source) =
    let half = size / 2
    // right[i] links cell i to the cell right of it, down[i] to the cell below it.
    let right = Array.zeroCreate<bool> cellCount
    let down = Array.zeroCreate<bool> cellCount
    let at x y = y * size + x

    for bx in 0 .. half - 1 do
        for by in 0 .. half - 1 do
            let x, y = 2 * bx, 2 * by
            right[at x y] <- true
            right[at x (y + 1)] <- true
            down[at x y] <- true
            down[at (x + 1) y] <- true

    let join (bx, by) (cx, cy) =
        if by = cy then
            let x, y = 2 * min bx cx + 1, 2 * by
            down[at x y] <- false
            down[at (x + 1) y] <- false
            right[at x y] <- true
            right[at x (y + 1)] <- true
        else
            let x, y = 2 * bx, 2 * min by cy + 1
            right[at x y] <- false
            right[at x (y + 1)] <- false
            down[at x y] <- true
            down[at (x + 1) y] <- true

    let visited = Array.zeroCreate<bool> (half * half)

    // Randomized depth-first search. At most 25 blocks deep, so the recursion stays small.
    let rec visit (bx, by) =
        visited[by * half + bx] <- true
        let steps = [| 1, 0; -1, 0; 0, 1; 0, -1 |]

        for i in steps.Length - 1 .. -1 .. 1 do
            let j = rng.Int(i + 1)
            let t = steps[i]
            steps[i] <- steps[j]
            steps[j] <- t

        for dx, dy in steps do
            let next = bx + dx, by + dy
            let nx, ny = next

            if nx >= 0 && nx < half && ny >= 0 && ny < half && not visited[ny * half + nx] then
                join (bx, by) next
                visit next

    visit (0, 0)

    let neighbors i =
        [ if right[i] then i + 1
          if i % size > 0 && right[i - 1] then i - 1
          if down[i] then i + size
          if i >= size && down[i - size] then i - size ]

    let cells = Array.zeroCreate<int> cellCount
    let mutable previous, current = -1, 0

    for k in 0 .. cellCount - 1 do
        cells[k] <- current
        let next = neighbors current |> List.find (fun c -> c <> previous)
        previous <- current
        current <- next

    ofCells cells

/// What the view draws for one move. Head, Fruit, and Target are cycle positions.
type Plan =
    { Head: int
      Fruit: int
      /// Where the snake goes.
      Target: int
      /// Head first.
      Body: Cell list }

/// Takes the neighboring cell furthest along the cycle that passes neither the fruit nor the tail.
/// The body then always lies in cycle order from tail to head, so every cell ahead of the head and before the
/// tail is free, and the next cell on the cycle is always safe.
let decide (c: Cycle) (g: Game) =
    let n = cellCount
    let position (cell: Cell) = c.Order[cell.Y * size + cell.X]
    // Steps forward along the cycle from a to b, wrapping past the last position.
    let forward a b = (b - a + n) % n
    let head = position (List.head g.Body)
    let tail = position (List.last g.Body)
    let fruit = position g.Fruit
    // A lone head has no tail yet. Counting it one step behind keeps the snake from eating the fruit just behind
    // it on the cycle: the tail would then be its next cell, and it cannot turn around into it.
    let toTail = if List.length g.Body = 1 then n - 1 else forward head tail
    let toFruit = forward head fruit
    let reach = max 1 (min toFruit (toTail - 1))

    let options =
        [ TurnLeft; Straight; TurnRight ]
        |> List.choose (fun t ->
            let cell = target t g

            if cell.X >= 0 && cell.X < size && cell.Y >= 0 && cell.Y < size then
                Some(t, forward head (position cell))
            else
                None)

    // The next cell on the cycle (1 step) is never behind the head once the snake is longer than 1.
    // A lone head may face away from it, and then no option is allowed. Any move is safe for a lone head,
    // and the shortest step keeps it from jumping far back.
    let turn, steps =
        match options |> List.filter (fun (_, k) -> k <= reach) with
        | [] -> options |> List.minBy snd
        | allowed -> allowed |> List.maxBy snd

    turn,
    { Head = head
      Fruit = fruit
      Target = (head + steps) % n
      Body = g.Body }
