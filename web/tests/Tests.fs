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
        [ test "the same brain and seed play the same game" {
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

          test "a circuit turns toward its more active steering neurons" {
              // Two steering neurons, one per side, each driven by its own input neuron.
              let circuit =
                  match
                      Connectome.build
                          { Neurons =
                              [| "in", "L", "input", 0.0, 0.0
                                 "in", "R", "input", 1.0, 0.0
                                 "DNa02", "L", "output", 0.0, 1.0
                                 "DNa02", "R", "output", 1.0, 1.0 |]
                            Inputs = [| 0; 1 |]
                            Left = [| 2, 1.0 |]
                            Right = [| 3, 1.0 |]
                            Edges = [| 0.0; 2.0; 1.0; 1.0; 3.0; 1.0 |]
                            Gaps = [||]
                            Cloud = [||]
                            Steps = 2 }
                  with
                  | Ok c -> c
                  | Error e -> failtest e

              let genome (left: float) (right: float) : Connectome.Genome =
                  // Snake input 0 drives the input neurons with the given strengths. Gain 1, no threshold.
                  { Encoder = Array.init Game.inputCount (fun i -> if i = 0 then [| left; right |] else [| 0.0; 0.0 |])
                    Gain = Array.zeroCreate 4
                    Threshold = Array.zeroCreate 4 }

              let input = Array.init Game.inputCount (fun i -> if i = 0 then 1.0 else 0.0)
              let turn l r = Connectome.decide circuit (Connectome.think circuit (genome l r) input)
              Expect.equal (turn 1.0 0.0) TurnLeft "left side more active"
              Expect.equal (turn 0.0 1.0) TurnRight "right side more active"
              Expect.equal (turn 1.0 1.0) Straight "a tie goes straight"
          }

          test "a gap junction carries activity to a silent partner" {
              // Input 0 drives neuron 0. Neuron 1 has no synapse, only a gap junction to neuron 0.
              let circuit gaps =
                  match
                      Connectome.build
                          { Neurons = [| "in", "", "input", 0.0, 0.0; "a", "", "output", 0.0, 0.0; "b", "", "output", 1.0, 0.0 |]
                            Inputs = [| 0 |]
                            Left = [| 1, 1.0 |]
                            Right = [| 2, 1.0 |]
                            Edges = [||]
                            Gaps = gaps
                            Cloud = [||]
                            Steps = 2 }
                  with
                  | Ok c -> c
                  | Error e -> failtest e

              let genome: Connectome.Genome =
                  { Encoder = Array.init Game.inputCount (fun i -> if i = 0 then [| 1.0 |] else [| 0.0 |])
                    Gain = Array.zeroCreate 3
                    Threshold = Array.zeroCreate 3 }

              let input = Array.init Game.inputCount (fun i -> if i = 0 then 1.0 else 0.0)
              let coupled = Connectome.think (circuit [| 0.0; 1.0; 0.5; 1.0; 0.0; 0.5 |]) genome input
              let alone = Connectome.think (circuit [||]) genome input
              Expect.isGreaterThan coupled[1] 0.0 "coupled neuron picks up activity"
              Expect.equal alone[1] 0.0 "without the gap junction it stays silent"
          }

          test "NEAT links always point forward, however the genomes grow" {
              // A cycle would make the net unevaluable. Disabled links count too: crossover can re-enable them.
              let rng = Rng.Source 3u
              let innovations = Neat.Innovations()
              let pool = Array.init 20 (fun _ -> Neat.random rng)

              for _ in 1..3000 do
                  let child = Neat.crossover innovations rng pool[rng.Int 20] pool[rng.Int 20]
                  let net = Neat.compile child

                  for j in 0 .. net.Ids.Length - 1 do
                      for e in net.Start[j] .. net.Start[j + 1] - 1 do
                          Expect.isLessThan net.Source[e] j "link points forward"

                  pool[rng.Int 20] <- child

              let grown = pool |> Array.map (fun g -> (Neat.compile g).Ids.Length - Neat.firstHidden) |> Array.max
              Expect.isGreaterThan grown 2 "genomes grew hidden neurons"
          }

          test "program trees survive the trip to the page" {
              let rng = Rng.Source 5u
              let mutable trees = List.init 50 (fun _ -> Program.random rng)

              for _ in 1..200 do
                  trees <- Program.crossover rng (List.item (rng.Int 50) trees) (List.item (rng.Int 50) trees) :: List.truncate 49 trees

              for tree in trees do
                  Expect.equal (Program.decode (Program.encode tree)) tree "round trip"
          }

          test "simplifying a program tree never changes a move" {
              let rng = Rng.Source 8u
              // Positions from games of random moves, so the tests see real senses.
              let positions =
                  [ for seed in 1u .. 30u do
                        let mutable now = Alive(Game.start seed)

                        while (match now with
                               | Alive _ -> true
                               | _ -> false) do
                            let g = Game.state now
                            yield vision g
                            let turn = [| TurnLeft; Straight; TurnRight |][rng.Int 3]
                            now <- step turn g ]

              for _ in 1..300 do
                  let tree = Program.random rng
                  let simple = Program.simplify tree

                  for input in positions do
                      Expect.equal (fst (Program.decide simple input)) (fst (Program.decide tree input)) "same move"
          }

          test "random Hamiltonian cycles visit every cell once and close" {
              // Hamilton.random checks its result with ofCells, which throws on a broken cycle.
              let rng = Rng.Source 11u

              for _ in 1..200 do
                  let c = Hamilton.random rng
                  Expect.equal (Hamilton.ofCells c.Cells) c "valid cycle"
          }

          test "the Hamiltonian cycle snake never crashes" {
              let rng = Rng.Source 12u

              for seed in 1u .. 20u do
                  let mind = Mind.Hamiltonian(Hamilton.random rng)
                  let mutable now = Alive(Game.start seed)

                  while (match now with
                         | Alive _ -> true
                         | _ -> false) do
                      let g = Game.state now
                      now <- step (Mind.think mind g).Turn g

                  match now with
                  | Dead((HitWall | HitSelf) as death, _) -> failtestf "seed %d: %A" seed death
                  | _ -> ()
          }

          test "the Q-table state sees danger and fruit from the snake's side" {
              // At the left wall facing North: West is the snake's left. Fruit 3 cells East is to its right.
              let g = place [ cell 0 5 ] North (cell 3 5) rng
              let danger, ahead, right = 4, 0, 1
              Expect.equal (QTable.state g) (danger * 9 + (ahead + 1) * 3 + (right + 1)) "danger left, fruit right"
          }

          test "a Q-table learns that a deadly move is bad" {
              // Facing a wall: going straight dies.
              let g = place [ cell 5 0 ] North (cell 9 9) rng
              let q = QTable.start (Rng.Source 1u)
              QTable.learn q g Straight (step Straight g)
              let s = QTable.state g
              Expect.isLessThan q[s * 3 + 1] 0.0 "straight into the wall lost value"
              Expect.notEqual (QTable.greedy q s) Straight "it no longer drives into the wall"
          }

          test "the snake starves without fruit" {
              let g = { place [ cell 5 5 ] North (cell 0 0) rng with Hunger = starveAfter 0 }

              match step Straight g with
              | Dead(Starved, _) -> ()
              | other -> failtestf "expected Starved, got %A" other
          } ]

[<EntryPoint>]
let main argv = runTestsWithCLIArgs [] argv tests
