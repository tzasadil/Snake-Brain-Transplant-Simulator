/// Tabular Q-learning: a table of how good each move is in each situation, learned during play.
/// The situation is compact on purpose, so every entry is visited often and the table fits on screen.
module SnakeEvo.QTable

open SnakeEvo.Rng
open SnakeEvo.Game

/// Danger left, ahead, right (8) times fruit direction (3 along the snake's axis, 3 across it).
let stateCount = 8 * 9

/// States that can occur: the fruit is never under the head, so 8 fruit directions, not 9.
let reachable = 8 * 8

/// Value of move a in state s at s * 3 + a. Moves in Left, Straight, Right order.
type Table = float[]

let private turns = [| TurnLeft; Straight; TurnRight |]

let turnIndex =
    function
    | TurnLeft -> 0
    | Straight -> 1
    | TurnRight -> 2

/// Rays 6, 0, and 2 point left, ahead, and right of the snake (Game.rayDirection).
let private sideRays = [| 6; 0; 2 |]

/// What the snake would sense with its head on `head`, facing its current heading, in the current game.
/// Called for every cell to draw the policy map, not only for the real head.
let stateAt (g: Game) (head: Cell) =
    let tail = List.last g.Body

    let blocked i =
        let dx, dy = rayDirection g sideRays[i]
        let x, y = head.X + dx, head.Y + dy
        // The tail leaves its cell this move, as in Game.step.
        x < 0 || x >= size || y < 0 || y >= size || (g.Occupied[y * size + x] && not (x = tail.X && y = tail.Y))

    let danger = (if blocked 0 then 4 else 0) + (if blocked 1 then 2 else 0) + (if blocked 2 then 1 else 0)
    let fx, fy = rayDirection g 0
    let rx, ry = rayDirection g 2
    let dx, dy = g.Fruit.X - head.X, g.Fruit.Y - head.Y
    let ahead = sign (dx * fx + dy * fy)
    let right = sign (dx * rx + dy * ry)
    danger * 9 + (ahead + 1) * 3 + (right + 1)

let state (g: Game) = stateAt g (List.head g.Body)

/// The best move in state s. Ties go straight first, so an untrained table drives ahead
/// instead of always turning one way.
let greedy (q: Table) s =
    let mutable best = 1

    for a in [ 0; 2 ] do
        if q[s * 3 + a] > q[s * 3 + best] then
            best <- a

    turns[best]

/// Learning rate, discount, and the share of random moves while learning.
let private alpha = 0.1
let private gamma = 0.9
let private epsilon = 0.05

/// Fruit and death matter most. The small credit for approaching gives a signal long before the first fruit,
/// like the approach credit in Evolution.trial.
let private reward =
    function
    | Ate -> Some 1.0
    | Died -> Some -1.0
    | Closer -> Some 0.1
    | Farther -> Some -0.1
    | Starvation -> None

let start (_: Source) : Table = Array.zeroCreate (stateCount * 3)

let act (rng: Source) (q: Table) (g: Game) =
    if rng.Float() < epsilon then turns[rng.Int 3] else greedy q (state g)

/// Q(s, a) <- Q(s, a) + alpha * (r + gamma * max Q(s', .) - Q(s, a)). The game's end has no future value.
let learn (q: Table) (before: Game) (turn: Turn) (after: Step) =
    match reward (feedback before after) with
    | None -> ()
    | Some r ->
        let k = state before * 3 + turnIndex turn

        let future =
            match after with
            | Alive g ->
                let s = state g
                gamma * max q[s * 3] (max q[s * 3 + 1] q[s * 3 + 2])
            | Dead _
            | Won _ -> 0.0

        q[k] <- q[k] + alpha * (r + future - q[k])

let freeze (q: Table) : Table = Array.copy q
