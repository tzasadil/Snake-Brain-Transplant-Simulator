/// Snake rules, ported from desktop/GameManager.fs as pure functions. Vision adds 3 inputs the original lacks.
module SnakeEvo.Game

open SnakeEvo.Native
open SnakeEvo.Rng

let size = 10

[<Struct>]
type Cell = { X: int; Y: int }

/// Clockwise from West. Vision and turning rely on this order.
type Heading =
    | West
    | North
    | East
    | South

type Turn =
    | TurnLeft
    | Straight
    | TurnRight

type Death =
    | HitWall
    | HitSelf
    | Starved

type Game =
    { /// Head first.
      Body: Cell list
      /// Mirrors Body, indexed by y * size + x. Makes collision and vision checks O(1).
      Occupied: bool[]
      Heading: Heading
      Fruit: Cell
      Score: int
      Turns: int
      /// Moves since the last fruit.
      Hunger: int
      Rng: Rng }

type Step =
    | Alive of Game
    | Dead of Death * Game
    /// The snake fills the board, so no fruit can spawn. Fruit stays under the head.
    | Won of Game

let state =
    function
    | Alive g
    | Dead(_, g)
    | Won g -> g

let private headings = [| West; North; East; South |]

let private headingIndex =
    function
    | West -> 0
    | North -> 1
    | East -> 2
    | South -> 3

let private index c = c.Y * size + c.X

/// Field compare. Fable's generic record equality was a measurable cost in the hot loop.
let private same (a: Cell) (b: Cell) = a.X = b.X && a.Y = b.Y

let private inBounds c =
    c.X >= 0 && c.X < size && c.Y >= 0 && c.Y < size

/// Moves without fruit before the snake starves. Without a limit a network scores forever by circling.
/// Longer snakes get more moves because they need longer paths to reach the fruit.
let starveAfter score = size * size / 2 + score * 2

/// Picks a random free cell. The original retried random cells, which loops forever on a full board.
let private spawnFruit (occupied: bool[]) rng =
    let free = [| for i in 0 .. occupied.Length - 1 do if not occupied[i] then i |]

    if free.Length = 0 then
        None, rng
    else
        let k, rng = nextInt free.Length rng
        Some { X = free[k] % size; Y = free[k] / size }, rng

/// Builds a position directly. Tests use it to set up exact boards.
let place (body: Cell list) heading fruit rng =
    let occupied = Array.zeroCreate<bool> (size * size)

    for c in body do
        occupied[index c] <- true

    { Body = body
      Occupied = occupied
      Heading = heading
      Fruit = fruit
      Score = 0
      Turns = 0
      Hunger = 0
      Rng = rng }

/// One snake cell in the center with a random heading, like the original.
let start seed =
    let h, rng = nextInt 4 (create seed)
    let head = { X = (size - 1) / 2; Y = (size - 1) / 2 }
    let g = place [ head ] headings[h] head rng

    match spawnFruit g.Occupied rng with
    | Some fruit, rng -> { g with Fruit = fruit; Rng = rng }
    | None, _ -> failwith "unreachable: a board with one snake cell has free cells"

let private offset =
    function
    | West -> -1, 0
    | North -> 0, -1
    | East -> 1, 0
    | South -> 0, 1

let private turn t h =
    let delta =
        match t with
        | TurnLeft -> 3
        | Straight -> 0
        | TurnRight -> 1

    headings[(headingIndex h + delta) % 4]

/// The cell the head enters on turn t. It may lie outside the board.
let target t g =
    let dx, dy = offset (turn t g.Heading)
    let head = List.head g.Body
    { X = head.X + dx; Y = head.Y + dy }

let step t g =
    if g.Hunger = starveAfter g.Score then
        Dead(Starved, g)
    else
        let heading = turn t g.Heading
        let next = target t g

        if not (inBounds next) then
            Dead(HitWall, g)
        elif same next g.Fruit then
            let occupied = Array.copy g.Occupied
            occupied[index next] <- true

            let grown =
                { g with
                    Body = next :: g.Body
                    Occupied = occupied
                    Heading = heading
                    Score = g.Score + 1
                    Turns = g.Turns + 1
                    Hunger = 0 }

            match spawnFruit occupied g.Rng with
            | Some fruit, rng -> Alive { grown with Fruit = fruit; Rng = rng }
            | None, _ -> Won grown
        else
            let tail = List.last g.Body

            // The tail leaves its cell this move, so entering it is safe.
            if at (index next) g.Occupied && not (same next tail) then
                Dead(HitSelf, g)
            else
                let occupied = Array.copy g.Occupied
                occupied[index tail] <- false
                occupied[index next] <- true

                Alive
                    { g with
                        Body = next :: List.truncate (List.length g.Body - 1) g.Body
                        Occupied = occupied
                        Heading = heading
                        Turns = g.Turns + 1
                        Hunger = g.Hunger + 1 }

/// What one move did, for brains that learn during play. A move that neither eats nor kills always changes the
/// distance to the fruit by one cell, so Closer and Farther cover every other move.
type Feedback =
    | Ate
    | Died
    /// Hunger ends the game before the chosen move happens, so that move is not to blame.
    | Starvation
    | Closer
    | Farther

let feedback (before: Game) (after: Step) =
    let distance g =
        let head = List.head g.Body
        abs (head.X - g.Fruit.X) + abs (head.Y - g.Fruit.Y)

    match after with
    | Dead(Starved, _) -> Starvation
    | Dead _ -> Died
    | Won _ -> Ate
    | Alive g when g.Score > before.Score -> Ate
    | Alive g -> if distance g < distance before then Closer else Farther

/// Ray directions clockwise from West.
let private rays = [| -1, 0; -1, -1; 0, -1; 1, -1; 1, 0; 1, 1; 0, 1; -1, 1 |]

/// Ray i, rotated with the snake: 0 forward, 2 right, 4 back, 6 left. Odd rays are diagonals.
let rayDirection g i =
    rays[(i + headingIndex g.Heading * 2) % 8]

let inputCount = 27

/// The network inputs.
/// - 0 to 23, as in the original: per ray, wall, own body, fruit, each as 1 / distance, or 0 when not seen.
///   The tail is invisible because it moves away before the head can reach it.
/// - 24, 25: fruit compass, the fruit offset along the snake's forward and right axes, divided by board size.
///   Rays see fruit only on 8 exact lines, so without it the snake wanders until the fruit lines up.
/// - 26: snake length as a share of the board, so strategy can change as the board fills.
/// Compass and length alone made scores worse under the original weak mutation.
/// With Gaussian mutation and turn balance (Brain.fs, Evolution.fs) they lifted the mean score
/// at generation 400 from 29 to 55 (6 seeds, 100 games each).
let vision g =
    let head = List.head g.Body
    let tail = List.last g.Body
    let input = Array.zeroCreate<float> inputCount

    for i in 0..7 do
        let dx, dy = rayDirection g i
        let mutable c = { X = head.X + dx; Y = head.Y + dy }
        let mutable dist = 1

        while inBounds c do
            if same c g.Fruit then
                if input[i * 3 + 2] = 0.0 then
                    input[i * 3 + 2] <- 1.0 / float dist
            elif at (index c) g.Occupied && not (same c tail) && input[i * 3 + 1] = 0.0 then
                input[i * 3 + 1] <- 1.0 / float dist

            c <- { X = c.X + dx; Y = c.Y + dy }
            dist <- dist + 1

        input[i * 3] <- 1.0 / float dist

    let fx, fy = offset g.Heading
    let rx, ry = offset (turn TurnRight g.Heading)
    let dx, dy = g.Fruit.X - head.X, g.Fruit.Y - head.Y
    input[24] <- float (dx * fx + dy * fy) / float size
    input[25] <- float (dx * rx + dy * ry) / float size
    input[26] <- float (List.length g.Body) / float (size * size)
    input
