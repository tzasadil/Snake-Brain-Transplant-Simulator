/// Seeded random numbers. xorshift32 is pure and tiny, so a seed alone fixes a whole game.
module SnakeEvo.Rng

/// xorshift32 state. Never 0, because xorshift maps 0 to itself forever.
[<Struct>]
type Rng = private Rng of uint32

let create (seed: uint32) =
    Rng(if seed = 0u then 0x9E3779B9u else seed)

/// Returns a float in [0, 1) and the next state.
let next (Rng x) =
    let x = x ^^^ (x <<< 13)
    let x = x ^^^ (x >>> 17)
    let x = x ^^^ (x <<< 5)
    float x / 4294967296.0, Rng x

/// Returns an int in [0, n). n = 0 gives 0, like System.Random.Next(0).
let nextInt (n: int) rng =
    let r, rng = next rng
    int (r * float n), rng

/// Mutable wrapper for evolution, which draws thousands of numbers per child.
/// Threading a pure state through nested weight arrays would bury the logic.
type Source(seed: uint32) =
    let mutable state = create seed

    member _.Float() =
        let r, s = next state
        state <- s
        r

    member this.Int(n: int) = int (this.Float() * float n)
    member this.Seed() = uint32 (this.Float() * 4294967296.0)
