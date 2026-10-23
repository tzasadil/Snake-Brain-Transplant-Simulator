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
              let a = Game.state (Evolution.play brain 12345u)
              let b = Game.state (Evolution.play brain 12345u)
              Expect.equal (a.Score, a.Turns, a.Body) (b.Score, b.Turns, b.Body) "same brain and seed"
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

          test "the snake starves without fruit" {
              let g = { place [ cell 5 5 ] North (cell 0 0) rng with Hunger = starveAfter 0 }

              match step Straight g with
              | Dead(Starved, _) -> ()
              | other -> failtestf "expected Starved, got %A" other
          } ]

[<EntryPoint>]
let main argv = runTestsWithCLIArgs [] argv tests
