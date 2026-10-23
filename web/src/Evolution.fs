/// Population and training loop, ported from Population.fs.
module SnakeEvo.Evolution

open SnakeEvo.Brain

let populationSize = 100

/// The best snakes join the next generation unchanged, so progress is never lost to bad children.
let private eliteCount = 2

type Champion =
    { Generation: int
      Brain: Brain
      /// Replaying the brain with this seed reproduces the exact game that won the generation.
      Seed: uint32
      Score: int }

type private Result =
    { Brain: Brain; Seed: uint32; Score: int }

/// Plays one game to the end. Same brain and seed => same game.
let play (brain: Brain) seed =
    let mutable step = Game.Alive(Game.start seed)

    while (match step with
           | Game.Alive _ -> true
           | _ -> false) do
        let g = Game.state step
        let output = Array.last (think brain (Game.vision g))
        step <- Game.step (decide output) g

    step

/// Trains in slices, so the browser can draw between them.
type Trainer(seed: uint32) =
    let rng = Rng.Source seed
    let mutable generation = 1
    let mutable brains = Array.init populationSize (fun _ -> random rng)
    let results = ResizeArray<Result>()

    let finishGeneration () =
        // Fitness is the score alone, as in the original.
        let ranked = results.ToArray() |> Array.sortByDescending (fun r -> r.Score)
        // Next(Next n) from the original: biased toward the top of the ranking.
        let parent () = ranked[rng.Int(rng.Int ranked.Length)].Brain

        brains <-
            Array.init populationSize (fun i ->
                if i < eliteCount then
                    ranked[i].Brain
                else
                    crossover rng (parent ()) (parent ()))

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
    member _.Train(hasTime: unit -> bool) =
        let champions = ResizeArray<Champion>()

        while hasTime () do
            let brain = brains[results.Count]
            let seed = rng.Seed()
            let score = (Game.state (play brain seed)).Score
            results.Add { Brain = brain; Seed = seed; Score = score }

            if results.Count = populationSize then
                champions.Add(finishGeneration ())

        List.ofSeq champions
