// Trains a circuit offline and stores the best genome in its file as "seed", so the browser starts from a snake
// that already eats. Many random worm populations never eat at all: every fitness is 0, so selection is blind
// and the population stays stuck.
// Run after extract_worm.py, from web/:  dotnet fsi tools/pretrain.fsx data/worm-circuit.json
#load "../src/Native.fs" "../src/Rng.fs" "../src/Game.fs" "../src/Brain.fs" "../src/Connectome.fs" "../src/Mind.fs" "../src/Evolution.fs"

open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open SnakeEvo

let path = fsi.CommandLineArgs[1]
let generations = 150
let seeds = [| 1u; 5u; 9u; 13u; 42u; 777u |]

let file = JsonNode.Parse(File.ReadAllText path).AsObject()
let doc = JsonDocument.Parse(File.ReadAllText path).RootElement
let numbers (e: JsonElement) = [| for v in e.EnumerateArray() -> v.GetDouble() |]
let pairs (e: JsonElement) = [| for p in e.EnumerateArray() -> let v = numbers p in int v[0], v[1] |]
let optional (name: string) = match doc.TryGetProperty name with | true, e -> numbers e | _ -> [||]

let circuit =
    match
        Connectome.build
            { Neurons =
                [| for n in doc.GetProperty("neurons").EnumerateArray() ->
                       let p = numbers (n.GetProperty "pos")
                       n.GetProperty("type").GetString(), n.GetProperty("side").GetString(), n.GetProperty("role").GetString(), p[0], p[1] |]
              Inputs = numbers (doc.GetProperty "inputs") |> Array.map int
              Left = pairs (doc.GetProperty "left")
              Right = pairs (doc.GetProperty "right")
              Edges = numbers (doc.GetProperty "edges")
              Gaps = optional "gaps"
              Cloud = [||]
              Steps = doc.GetProperty("steps").GetInt32()
              // Pretraining always starts from scratch, even if the file already holds a seed.
              Seed = None }
    with
    | Ok c -> c
    | Error e -> failwith e

/// Mean score over 100 fixed games, so champions compare fairly. A champion's own training score is a lucky maximum.
let evaluate (g: Connectome.Genome) =
    let mind = Mind.Wired(circuit, g)

    [ for s in 1u .. 100u ->
          let mutable step = Game.Alive(Game.start (s * 7919u))

          while (match step with
                 | Game.Alive _ -> true
                 | _ -> false) do
              let game = Game.state step
              step <- Game.step (Mind.think mind (Game.vision game)).Turn game

          float (Game.state step).Score ]
    |> List.average

let best =
    seeds
    |> Array.Parallel.map (fun seed ->
        let trainer = Evolution.Trainer(seed, Evolution.connectome circuit)
        let champions = [ for gen in 1..generations -> trainer.Train(fun () -> trainer.Generation = gen) |> List.exactlyOne ]

        champions
        |> List.filter (fun c -> c.Generation % 10 = 0)
        |> List.map (fun c -> evaluate c.Brain, c.Brain)
        |> List.maxBy fst
        |> fun (score, genome) ->
            printfn $"seed {seed}: best champion scores {score:F1}"
            score, genome)
    |> Array.maxBy fst

let round4 (xs: float[]) = JsonArray([| for x in xs -> JsonValue.Create(System.Math.Round(x, 4)) :> JsonNode |])
let score, genome = best

file["seed"] <-
    JsonObject(
        dict
            [ "encoder", JsonArray([| for row in genome.Encoder -> round4 row :> JsonNode |]) :> JsonNode
              "gain", round4 genome.Gain
              "threshold", round4 genome.Threshold
              "score", JsonValue.Create(System.Math.Round(score, 1)) ]
    )

File.WriteAllText(path, file.ToJsonString())
printfn $"seed genome scoring {score:F1} written to {path}"
