# Experiment reports

Short learning runs behind the claims about each brain mode.

## Method

- Runs use `web/tools/experiment.fsx` on .NET. Since evolution ranks snakes with a stable sort, .NET and the browser play exactly the same games, so the results carry over.
- Score: each seed's champion plays 20 fixed games. The tables show the mean fruit per game, averaged over seeds. A champion's own training score is a lucky maximum, so only fixed games compare fairly.
- Seeds 1, 42, 5, and 9, unless the row says otherwise. The spread between seeds is large: at generation 60 a fly seed may score anywhere from 3 to 23. Differences of 1 or 2 fruit are noise.

From `web/`, for example:

```sh
dotnet fsi tools/experiment.fsx net real 1,42,5,9 60
dotnet fsi tools/experiment.fsx data/fly-circuit.json random 1,42,5,9 60 tuned
```

Arguments: brain, wiring (`real`, `random`, or `signs`), seeds, generations, weights (`fixed`, `tuned`, or `free`). The script header explains each control.

## Fruit fly: does the wiring matter?

Controls:
- **random**: the same neurons and edge count, with random senders and receivers.
- **tuned**: adds one strength gene per synapse, so signs stay fixed.
- **free**: also adds a sign gene per synapse.

| Wiring | fixed | tuned | free |
|---|---|---|---|
| real | 10.7 | 13.6 | 9.6 |
| random | 11.8 | 13.2 | 11.6 |

Mean fruit at generation 60. Fixed and tuned use 36 seeds (1 to 11, 42, and 12 to 35). Free uses 12 seeds.

- The random circuit learns as well as the real one. The evolved encoder, gains, and thresholds do most of the work, not the fly's wiring.
- Tuned strengths help both circuits by about the same amount, so the wiring still shows no advantage.
- Free signs do not help. They add about 2,249 genes, which spreads the mutations thinner.

## Worm

| Seed | 1 | 42 | 5 | 9 |
|---|---|---|---|---|
| Fruit at generation 60 | 15.6 | 11.6 | 11.5 | 17.5 |

Without the partial credit for approaching fruit, some populations never ate. Selection was then blind, and they stayed stuck.

## Neural net and NEAT

| Brain | Generation 60 | Generation 140 |
|---|---|---|
| Neural net | 19.9 | |
| NEAT | 22.9 | 34.5 |
| NEAT without growth | 26.7 | 35.0 |

"NEAT without growth" sets `addLinkChance` and `addNodeChance` to 0 in `web/src/Neat.fs`. Only the weights of the direct input-to-output wiring evolve.

- NEAT beats the network, but the version that never grows does just as well. The gain comes from the simple direct wiring, not from grown neurons.
- NEAT usually protects new structure with speciation, which this version lacks. See the `TODO:` in `web/src/Neat.fs`.

## Program tree

| Version | Fruit at generation 60 |
|---|---|
| Full | 15.3 |
| Without the `Blocked` test | 13.3 |
| Without the insert mutation | 12.0 |

The variants change `randomTest` and `insertChance` in `web/src/Program.fs`.

- Without `Blocked`, trees chase the fruit but run into their own body. The fitting `Within` test, body within 1 cell ahead, is about a 1 in 230 draw.
- The insert mutation wraps a branch in a new test, so a guard like "blocked ahead?" can go above a tree that already chases fruit in one step.

## Hamiltonian cycle

200 random cycles, one game each: the snake filled the board every time (score 99), in about 2,300 moves on average. It never hit a wall or itself, and it never starved. This mode does not learn, so it has no generations to compare.
