/// Genetic programming: the brain is a readable decision tree, such as "if fruit ahead then straight".
/// Evolution swaps branches between parents and rewrites single tests and moves.
module SnakeEvo.Program

open SnakeEvo.Rng
open SnakeEvo.Game

type Thing =
    | Wall
    | Body
    | Fruit

type Side =
    | Ahead
    | Behind
    | Left
    | Right

/// Tests read the same senses as the network, so every brain sees the same world.
/// Thresholds are whole cells, so a rule reads exactly and compares exactly in JS and .NET.
type Test =
    /// The next cell on a ray is wall or body. Danger checks matter most, and as Within tests they were a rare draw.
    | Blocked of ray: int
    /// The thing is on a ray (0 forward, then clockwise, rotated with the snake) at most this many cells away.
    | Within of ray: int * Thing * cells: int
    /// The fruit compass points to this side.
    | FruitTo of Side
    /// The snake is longer than this many cells.
    | Longer of cells: int

type Tree =
    | Do of Turn
    | If of Test * yes: Tree * no: Tree

/// Deeper trees no longer fit the brain panel, and grow without playing better (bloat).
let maxDepth = 4

let private things = [| Wall; Body; Fruit |]
let private sides = [| Ahead; Behind; Left; Right |]
let private turns = [| TurnLeft; Straight; TurnRight |]
let private maxCells = size - 1

let holds (input: float[]) =
    function
    | Blocked ray -> input[ray * 3] >= 1.0 || input[ray * 3 + 1] >= 1.0
    | Within(ray, thing, cells) ->
        let k =
            match thing with
            | Wall -> 0
            | Body -> 1
            | Fruit -> 2

        // Rays report 1 / distance, or 0 when nothing is seen.
        input[ray * 3 + k] >= 1.0 / float cells
    | FruitTo Ahead -> input[24] > 0.0
    | FruitTo Behind -> input[24] < 0.0
    | FruitTo Right -> input[25] > 0.0
    | FruitTo Left -> input[25] < 0.0
    | Longer cells -> round (input[26] * float (size * size)) > float cells

/// The turn, and the answer to each test on the way down: true took the yes branch.
let decide (tree: Tree) (input: float[]) =
    let rec walk tree path =
        match tree with
        | Do turn -> turn, List.rev path
        | If(test, yes, no) ->
            if holds input test then walk yes (true :: path) else walk no (false :: path)

    walk tree []

/// Near distances are likelier: they matter more for the next move.
let private randomTest (rng: Source) =
    match rng.Int 4 with
    | 0 -> Blocked(rng.Int 8)
    | 1 -> Within(rng.Int 8, things[rng.Int 3], 1 + rng.Int(1 + rng.Int maxCells))
    | 2 -> FruitTo sides[rng.Int 4]
    | _ -> Longer(1 + rng.Int(size * size / 2))

/// Grows a random tree at most `depth` tests deep. Some branches stop early, so shapes vary.
let rec private grow (rng: Source) depth =
    if depth = 0 || rng.Float() < 0.3 then
        Do turns[rng.Int 3]
    else
        If(randomTest rng, grow rng (depth - 1), grow rng (depth - 1))

let random (rng: Source) = grow rng 3

let rec private depth =
    function
    | Do _ -> 0
    | If(_, yes, no) -> 1 + max (depth yes) (depth no)

let rec private count =
    function
    | Do _ -> 1
    | If(_, yes, no) -> 1 + count yes + count no

/// Node i in preorder.
let rec private get i tree =
    match tree with
    | _ when i = 0 -> tree
    | Do _ -> failwith "unreachable: index past the tree"
    | If(_, yes, no) ->
        let n = count yes
        if i <= n then get (i - 1) yes else get (i - 1 - n) no

let rec private replace i (sub: Tree) tree =
    match tree with
    | _ when i = 0 -> sub
    | Do _ -> failwith "unreachable: index past the tree"
    | If(test, yes, no) ->
        let n = count yes
        if i <= n then If(test, replace (i - 1) sub yes, no) else If(test, yes, replace (i - 1 - n) sub no)

/// Drops tests that cannot change the move: both branches equal, or a test repeated inside its own branch.
/// Keeps trees short and readable.
let rec simplify tree =
    let rec assume test answer =
        function
        | If(t, yes, no) when t = test -> assume test answer (if answer then yes else no)
        | If(t, yes, no) -> If(t, assume test answer yes, assume test answer no)
        | leaf -> leaf

    match tree with
    | Do _ -> tree
    | If(test, yes, no) ->
        let yes = simplify (assume test true yes)
        let no = simplify (assume test false no)
        if yes = no then yes else If(test, yes, no)

let private pointChance = 0.3
let private subtreeChance = 0.1
let private insertChance = 0.2

/// Rewrites one node: a move becomes another move, a test another test with the same branches.
let private point (rng: Source) tree =
    let i = rng.Int(count tree)

    let node =
        match get i tree with
        | Do _ -> Do turns[rng.Int 3]
        | If(_, yes, no) -> If(randomTest rng, yes, no)

    replace i node tree

/// Wraps one node in a new test with a random move on the other side, so a guard such as "blocked ahead?"
/// can go above a tree that already chases fruit in one step. Without it, trees reached about 12 fruit by generation 60, with it about 15 (4 seeds).
let private insert (rng: Source) tree =
    let i = rng.Int(count tree)
    let node = get i tree
    let other = Do turns[rng.Int 3]
    let test = randomTest rng
    replace i (if rng.Float() < 0.5 then If(test, node, other) else If(test, other, node)) tree

/// Child = parent a with one of its branches swapped for a branch of b, then maybe a rewritten node,
/// an inserted test, or a regrown branch. Children too deep for the panel fall back to the step before.
let crossover (rng: Source) (a: Tree) (b: Tree) =
    let child = replace (rng.Int(count a)) (get (rng.Int(count b)) b) a
    let child = if depth child > maxDepth then a else child
    let child = if rng.Float() < pointChance then point rng child else child

    let child =
        if rng.Float() < insertChance then
            let wrapped = insert rng child
            if depth wrapped > maxDepth then child else wrapped
        else
            child

    let child =
        if rng.Float() < subtreeChance then
            let regrown = replace (rng.Int(count child)) (grow rng 2) child
            if depth regrown > maxDepth then child else regrown
        else
            child

    simplify child

let private turnIndex =
    function
    | TurnLeft -> 0
    | Straight -> 1
    | TurnRight -> 2

/// Flat preorder numbers for the worker message, since structured clone breaks unions.
/// Move: 0, turn. Test: 1, then 0 ray thing cells | 1 side | 2 cells | 3 ray, then the yes and no branches.
let encode tree =
    let out = ResizeArray<int>()

    let rec go =
        function
        | Do turn -> out.AddRange [ 0; turnIndex turn ]
        | If(test, yes, no) ->
            out.Add 1

            match test with
            | Within(ray, thing, cells) -> out.AddRange [ 0; ray; Array.findIndex ((=) thing) things; cells ]
            | FruitTo side -> out.AddRange [ 1; Array.findIndex ((=) side) sides ]
            | Longer cells -> out.AddRange [ 2; cells ]
            | Blocked ray -> out.AddRange [ 3; ray ]

            go yes
            go no

    go tree
    out.ToArray()

/// Inverse of encode. Fails on malformed input: the worker builds it, so a failure is a bug, not user error.
let decode (code: int[]) =
    let mutable i = 0

    let next () =
        if i >= code.Length then failwith "program code ends early"
        i <- i + 1
        code[i - 1]

    let pick (items: 'T[]) k =
        if k >= 0 && k < items.Length then items[k] else failwith $"program code has bad index {k}"

    let rec go () =
        match next () with
        | 0 -> Do(pick turns (next ()))
        | 1 ->
            let test =
                match next () with
                | 0 ->
                    let ray = next ()
                    let thing = pick things (next ())
                    Within(pick [| 0..7 |] ray, thing, next ())
                | 1 -> FruitTo(pick sides (next ()))
                | 2 -> Longer(next ())
                | 3 -> Blocked(pick [| 0..7 |] (next ()))
                | k -> failwith $"program code has unknown test {k}"

            let yes = go ()
            If(test, yes, go ())
        | k -> failwith $"program code has unknown node {k}"

    let tree = go ()
    if i <> code.Length then failwith "program code has extra numbers"
    tree
