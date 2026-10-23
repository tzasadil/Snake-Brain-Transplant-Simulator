module SnakeEvo.Tests

open Expecto
open SnakeEvo
open SnakeEvo.Game

let private cell x y = { X = x; Y = y }
let private rng = Rng.create 1u

let private alive =
    function
    | Alive g -> g
    | other -> failtestf "expected Alive, got %A" other

let tests =
    testList
        "snake"
        [ test "a champion replays its exact game" {
              let brain = Brain.random (Rng.Source 7u)
              let mind = Mind.Net brain
              Expect.equal (Evolution.trial mind 12345u) (Evolution.trial mind 12345u) "same brain and seed"
          }

          test "the fruit compass rotates with the snake" {
              // Facing East at (2,5): fruit at (6,5) is 4 ahead. Right of East is South.
              let ahead = vision (place [ cell 2 5 ] East (cell 6 5) rng)
              Expect.equal (ahead.[24], ahead.[25]) (0.4, 0.0) "fruit ahead"
              let south = vision (place [ cell 2 5 ] East (cell 2 8) rng)
              Expect.equal (south.[24], south.[25]) (0.0, 0.3) "fruit to the right"
              let behindLeft = vision (place [ cell 5 5 ] North (cell 3 7) rng)
              Expect.equal (behindLeft.[24], behindLeft.[25]) (-0.2, -0.2) "fruit behind and left when facing North"
          }

          test "ray 0 looks forward and rotates with the snake" {
              // Head at x=2 facing East: the wall is 8 cells ahead.
              let east = place [ cell 2 5 ] East (cell 0 0) rng
              Expect.equal (vision east).[0] (1.0 / 8.0) "wall ahead when facing East"
              let north = alive (step TurnLeft east)
              // Now at (2,4) facing North: the wall is 5 cells ahead.
              Expect.equal north.Heading North "turned left"
              Expect.equal (vision north).[0] (1.0 / 5.0) "wall ahead when facing North"
          }

          test "the head may enter the cell the tail leaves" {
              // A 2x2 loop: head (1,1), tail (1,0). Heading West, turning right leads North into the tail.
              let g = place [ cell 1 1; cell 0 1; cell 0 0; cell 1 0 ] West (cell 9 9) rng
              Expect.equal (vision g).[2 * 3 + 1] 0.0 "the tail is invisible to the right ray"
              let moved = alive (step TurnRight g)
              Expect.equal (List.head moved.Body) (cell 1 0) "head moved onto the old tail"
          }

          test "hitting the body kills" {
              // Same loop, but the tail at (1,0) is replaced by a longer body, so (1,0) stays occupied.
              let g = place [ cell 1 1; cell 0 1; cell 0 0; cell 1 0; cell 2 0 ] West (cell 9 9) rng

              match step TurnRight g with
              | Dead(HitSelf, _) -> ()
              | other -> failtestf "expected HitSelf, got %A" other
          }

          test "the fly turns toward its more active steering neurons" {
              // Two steering neurons, one per side, each driven by its own input neuron.
              let neurons =
                  [| "in", "L", "input", 0.0, 0.0
                     "in", "R", "input", 1.0, 0.0
                     "DNa02", "L", "output", 0.0, 1.0
                     "DNa02", "R", "output", 1.0, 1.0 |]

              let circuit =
                  match Fly.build neurons [| 0; 1 |] [| 2, 1.0 |] [| 3, 1.0 |] [| 0.0; 2.0; 1.0; 1.0; 3.0; 1.0 |] [||] with
                  | Ok c -> c
                  | Error e -> failtest e

              let genome (left: float) (right: float) : Fly.Genome =
                  // Snake input 0 drives the input neurons with the given strengths. Gain 1, no threshold.
                  { Encoder = Array.init Game.inputCount (fun i -> if i = 0 then [| left; right |] else [| 0.0; 0.0 |])
                    Gain = Array.zeroCreate 4
                    Threshold = Array.zeroCreate 4 }

              let input = Array.init Game.inputCount (fun i -> if i = 0 then 1.0 else 0.0)
              let turn l r = Fly.decide circuit (Fly.think circuit (genome l r) input)
              Expect.equal (turn 1.0 0.0) TurnLeft "left side more active"
              Expect.equal (turn 0.0 1.0) TurnRight "right side more active"
              Expect.equal (turn 1.0 1.0) Straight "a tie goes straight"
          }

          test "the snake starves without fruit" {
              let g = { place [ cell 5 5 ] North (cell 0 0) rng with Hunger = starveAfter 0 }

              match step Straight g with
              | Dead(Starved, _) -> ()
              | other -> failtestf "expected Starved, got %A" other
          } ]

[<EntryPoint>]
let main argv = runTestsWithCLIArgs [] argv tests
