/// Population and training loop, ported from Population.fs. Fitness adds a turn balance rule the original lacks.
module SnakeEvo.Evolution

open SnakeEvo.Mind

let populationSize = 100

/// The best snakes join the next generation unchanged, so progress is never lost to bad children.
let private eliteCount = 2

/// What evolution needs from one kind of brain. A trainer holds one species, so genomes never mix.
type Species<'G> =
    { Random: Rng.Source -> 'G
      Crossover: Rng.Source -> 'G -> 'G -> 'G
      Mind: 'G -> Mind }

let net =
    { Random = Brain.random
      Crossover = Brain.crossover
      Mind = Net }

let connectome circuit =
    { Random = Connectome.random circuit
      Crossover = Connectome.crossover
      Mind = fun genome -> Wired(circuit, genome) }

type Champion<'G> =
    { Generation: int
      Brain: 'G
      /// Replaying the brain with this seed reproduces the exact game that won the generation.
      Seed: uint32
      Score: int }

type private Result<'G> =
    { Brain: 'G
      Seed: uint32
      Score: int
      Fitness: float }

/// The rarer turn side needs this share of turns for full fitness.
/// Without it every population settles on turning one way only, and the most one-sided stall around score 26.
let private minTurnShare = 0.25

/// Plays one game to the end. Same brain and seed => same game.
/// Fitness is the score, scaled down when the snake turns one way more than 3:1.
/// A penalty, not a hard limit: early in a game one turn is 100% one side, and a snake overridden by the rules never learns.
let trial (mind: Mind) seed =
    let mutable step = Game.Alive(Game.start seed)
    let mutable lefts, rights = 0, 0

    while (match step with
           | Game.Alive _ -> true
           | _ -> false) do
        let g = Game.state step
        let turn = (think mind (Game.vision g)).Turn

        match turn with
        | Game.TurnLeft -> lefts <- lefts + 1
        | Game.TurnRight -> rights <- rights + 1
        | Game.Straight -> ()

        step <- Game.step turn g

    let score = (Game.state step).Score
    let share = if lefts + rights = 0 then 0.0 else float (min lefts rights) / float (lefts + rights)
    score, float score * min 1.0 (share / minTurnShare)

/// Trains in slices, so the browser can draw between them.
type Trainer<'G>(seed: uint32, species: Species<'G>) =
    let rng = Rng.Source seed
    let mutable generation = 1
    let mutable brains = Array.init populationSize (fun _ -> species.Random rng)
    let results = ResizeArray<Result<'G>>()

    let finishGeneration () =
        let ranked = results.ToArray() |> Array.sortByDescending (fun r -> r.Fitness)
        // Next(Next n) from the original: biased toward the top of the ranking.
        let parent () = ranked[rng.Int(rng.Int ranked.Length)].Brain

        brains <-
            Array.init populationSize (fun i ->
                if i < eliteCount then
                    ranked[i].Brain
                else
                    species.Crossover rng (parent ()) (parent ()))

        results.Clear()
        let best = ranked[0]

        let champion =
            { Generation = generation
              Brain = best.Brain
              Seed = best.Seed
              Score = best.Score }

        generation <- generation + 1
        champion

    /// The generation in progress.
    member _.Generation = generation

    /// Plays games while `hasTime ()` holds. Returns the champions of the generations it finished.
    /// Every game gets a fresh seed, so elites face new fruit and the best score can drop, like the original.
    /// Fitness averages 2 games to cut the luck of fruit placement. The first game is the one a replay shows.
    member _.Train(hasTime: unit -> bool) =
        let champions = ResizeArray<Champion<'G>>()

        while hasTime () do
            let brain = brains[results.Count]
            let seed = rng.Seed()
            let mind = species.Mind brain
            let score, fitness = trial mind seed
            let _, second = trial mind (rng.Seed())

            results.Add
                { Brain = brain
                  Seed = seed
                  Score = score
                  Fitness = (fitness + second) / 2.0 }

            if results.Count = populationSize then
                champions.Add(finishGeneration ())

        List.ofSeq champions
