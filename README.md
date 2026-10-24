# Snake Brain Transplant Simulator

A Snake game in F# where a genetic algorithm evolves neural networks that learn to play it. You can play yourself, train a population, or watch the best snake of any past generation.

**[Watch it evolve in your browser](https://tzasadil.github.io/Snake-Brain-Transplant-Simulator/)**

![Generation 1 starves, then generation 74 hunts fruit while its network lights up](docs/demo.gif)

## How it works

- **Game**: 10x10 grid. The snake grows when it eats fruit and dies when it hits a wall or itself. It also starves if it goes too long without food, so a network cannot score by circling forever.
- **Senses**: the snake looks in 8 directions and measures the distance to the nearest wall, its own body, and fruit (24 inputs). The view rotates with the snake, so "left" always means the snake's left. The browser version adds a fruit compass (fruit offset ahead and to the right) and the snake's length, 27 inputs in total.
- **Brain**: a small multi-layer perceptron, 24 -> 18 -> 3 neurons with ReLU. The 3 outputs are turn left, go straight, and turn right.
- **Evolution**: 100 snakes play one game each in parallel. The two best survive unchanged (elitism). Fitter snakes are more likely to be picked as parents. Children mix parent weights at random, then mutate a few of them. The browser version mutates with Gaussian noise and averages 2 games per snake, with the score cut when the snake turns one way more than 3:1, plus partial credit (less than one fruit) for moving toward the fruit, so a population that has not eaten yet still has something to select on.
- **Browser**: the same rules, compiled to JavaScript with Fable. Training runs in a Web Worker. The page plays each generation's best snake on a fresh random game and shows which neurons drive each move.
- **NEAT mode** (browser): evolution grows the network, as in NEAT (Stanley and Miikkulainen 2002). Every snake starts with its senses wired straight to the 3 outputs. A child can add a link or split a link into a new neuron. Links are matched by their two endpoints instead of NEAT's innovation numbers, and there is no speciation.
- **Program tree mode** (browser): genetic programming. The brain is a readable decision tree of tests such as "blocked ahead?" or "fruit to the left?", ending in moves. Children swap branches between parents, rewrite single tests or moves, or wrap a branch in a new test. Trees stay at most 4 tests deep, so they fit the panel, and tests that cannot change the move are pruned. The view lights the tests on the way to the current move.
- **Q-table mode** (browser): reinforcement learning, no evolution. One snake learns while it plays. It knows only danger to its left, ahead, and right, and which way the fruit is: 64 situations with 3 moves each. After every move it nudges that move's value toward the reward plus the best value of the next situation (Q-learning). Fruit is worth +1, death -1, a step toward the fruit +0.1, and a step away -0.1. 5% of moves are random, so it keeps exploring. After each game, a frozen copy plays one test game: that is one bar on the chart, and the board plays these copies. The view shows the move the table would pick in every cell.
- **Spiking + dopamine mode** (browser): spiking neurons that learn during play, no evolution. The senses drive 29 input neurons by firing rate. These feed 40 leaky integrate-and-fire hidden neurons over fixed random wiring, and inputs and hidden neurons both feed 3 output neurons. Each move runs 20 time steps, and the move is the output that fires most. Every synapse onto an output keeps a fading trace of recent activity. Fruit, death, and steps toward or away from the fruit release a dopamine-like signal: above its running average it strengthens traced synapses, below it weakens them. The rule is REINFORCE (Williams 1992) over spike counts, not spike-timing plasticity. While learning, the snake picks its move at random, weighted by how strongly each output fires. The view is a scrolling spike raster of the last 10 moves.
- **Hamiltonian cycle mode** (browser): no learning, a classic algorithm for comparison. The page builds a random cycle through every cell exactly once and numbers the cells along it. The snake follows the cycle, but it may jump to the neighboring cell furthest along it, as long as the jump passes neither the fruit nor the tail. The body then always lies in cycle order, so the snake never hits itself. `New cycle` draws another cycle.
- **Fruit fly mode** (browser): a real fly brain circuit plays instead of the network. A script extracts 251 neurons and 2,249 synapse bundles from the male fruit fly connectome. It keeps whole paths: the strongest routes from visual projection neurons to the steering neurons DNa01 and DNa02, where a path's strength is the product of each connection's share of its target's input. After each pick it weakens that path's connections, so later picks find new routes (262 paths). Visual neurons wired straight onto steering are left out, so every signal crosses the brain. Wiring, signs, and synapse counts stay fixed. Evolution tunes only how the snake's senses feed the visual neurons, plus a gain and threshold per neuron, as in [ChessFly](https://huggingface.co/mlabonne/chessfly). The snake turns toward the side whose steering neurons fire more.
- **Worm mode** (browser): the whole nervous system of the roundworm C. elegans plays: 299 neurons, 1,922 chemical synapses, and 1,079 gap junctions. The snake's senses feed 36 head sensory neurons, and the snake turns toward whichever head motor neurons fire more, dorsal (SMDD, RMDD) for left or ventral (SMDV, RMDV) for right, as the worm steers by bending its head. Signs come from each neuron's transmitter: GABA inhibits, acetylcholine and glutamate excite. Evolution tunes the same encoder, gains, and thresholds as in fruit fly mode. Restart draws a new population.
- **Desktop GUI**: WPF in strict MVVM. `PropertyChanged.Fody` injects `INotifyPropertyChanged`, so the view models stay plain F#.

How well each mode learns, and the controls behind those claims: [experiment reports](docs/experiment-reports.md).

## What I learned

- A random population either spins in place or drives into a wall. It takes several generations before snakes start chasing fruit.
- Snake gets harder as the snake grows. Unlike Tetris or Flappy Bird, speed does not set the difficulty, so the network needs a different strategy late in the game.
- Fruit on edges and in corners is the main failure. Many evolved snakes learn to patrol the edges.
- Most populations learn to turn in only one direction, and the most one-sided stall around score 26. A fitness penalty for turning one way more than 3:1 fixes that.
- The original mutation (a fixed step of 0.02) was too weak to tune new inputs, so adding inputs made snakes worse. With Gaussian mutation, turn balance, a fruit compass, and length, the mean score at generation 400 rose from 29 to 55 (6 seeds, 100 games each).
- Late-game deaths come from limited vision: the snake only sees along 8 rays, not the whole board.
- Fable 5 checks the bounds of every array read in a library call. In the browser that made training 17x slower than on .NET. Plain JS indexing in the hot loops cut the gap to about 4x.

## Run

### Browser

Needs the .NET 10 SDK and Node.js with pnpm.

1. Go to `web/`.
2. Run `dotnet tool restore`.
3. Run `pnpm install`.
4. Run `pnpm start` and open the printed URL.

`pnpm build` writes the static site to `web/dist/`. `pnpm test` runs the rule tests.

| Control | Action |
|---|---|
| `Brain` | Fruit fly (default), worm, neural net, NEAT, program tree, Q-table, spiking + dopamine, or Hamiltonian cycle. Switching restarts training. |
| Score chart | Training score of each generation's best snake. Click to watch it play a new game. Arrow keys work too. |
| `Pause` / `Resume` | Stop or continue training. It also pauses every 300 generations. |
| `Follow latest` | Each new champion takes over the game in progress |
| `⏸` / `▶` | Stop or resume the game on the board. Training goes on. |
| `Speed` | Playback speed |
| `Restart` | Train a new random population |
| `New cycle` | Hamiltonian cycle mode only, which does not train: draw a new cycle |

### Desktop

Windows only (.NET Framework 4.8, WPF).

1. Open `snake_Fs.sln` in Visual Studio.
2. Restore NuGet packages.
3. Run the project.

| Control | Action |
|---|---|
| `Space` | Pause or resume |
| `W` `A` `S` `D` | Steer the snake in human mode |
| `Human` / `Bot` | Switch between you and the AI |
| `Train` | Train the current population, click again to stop |
| `Save` / `Load` | Store or restore a population as a folder of JSON networks |
| Training history row | Let the best snake of that generation play, even mid-game |

## Code map

| File | Role |
|---|---|
| `GameManager.fs` | Game loop, collisions, and the 8-ray vision |
| `AI.fs` | Network, forward pass, crossover, and mutation |
| `Population.fs` | Parallel evaluation, selection, and the training loop |
| `Settings.fs` | Grid size, network shape, and mutation rates |
| `MainWindow.xaml.fs` | View model and commands |
| `ExampleSnake.fs` | A pretrained snake, noted for its jiggly style |
| `web/src/Game.fs` | Browser port of the rules and vision, as pure functions |
| `web/src/Brain.fs` | Browser port of the network, crossover, and mutation |
| `web/src/Evolution.fs` | Browser port of selection and the training loop |
| `web/src/Connectome.fs` | Fly and worm circuits: wiring, gap junctions, neuron dynamics, steering readout, and genome |
| `web/src/Neat.fs` | NEAT: growing genomes, crossover by link, and compiling a genome for fast play |
| `web/src/Program.fs` | Program trees: tests, evaluation, crossover, mutation, and the flat form sent to the page |
| `web/src/Hamilton.fs` | Random Hamiltonian cycles and the shortcut rule |
| `web/src/QTable.fs` | Q-learning: situations, values, and the update after each move |
| `web/src/Spiking.fs` | Spiking neurons, dopamine, and the trace that links them |
| `web/src/Mind.fs` | Every kind of brain behind one interface |
| `web/tools/extract_fly.py` | Builds `web/data/fly-circuit.json` from the connectome download |
| `web/tools/extract_worm.py` | Builds `web/data/worm-circuit.json` from OpenWorm data |
| `web/tools/experiment.fsx` | Short learning runs on .NET, with random-wiring, shuffled-sign, and tunable-weight controls |
| `web/tools/check-page.mjs` | Headless Chrome check: trains a brain, samples stats, screenshots, reports errors |
| `web/tools/source-data/` | Raw connectome downloads, gitignored. The extraction scripts list their sources. |
| `web/src/Worker.fs` | Trains off the main thread |
| `web/src/View.fs` | Draws the board, the network, and the score chart |
| `web/src/App.fs` | Playback loop and page controls |

[Architecture diagram](https://drive.google.com/file/d/1WRbTG-MDYLCUJCyy3G-27LuXoD9cK_V1/view?usp=sharing)

Built at Charles University.

## Credits

Fruit fly mode uses the Male CNS v1.0 connectome by FlyEM at HHMI Janelia, the University of Cambridge, the MRC Laboratory of Molecular Biology, and Google Research, licensed [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Source: [male-cns.janelia.org](https://male-cns.janelia.org). `web/data/fly-circuit.json` is a subset of it.

Worm mode uses the C. elegans wiring of Varshney et al. 2011 (PLoS Comput Biol, CC BY), neurotransmitter identities from Wang et al. 2024 (eLife, CC BY), and soma positions from the WormBase Virtual Worm (public domain), all as redistributed by [OpenWorm](https://openworm.org) (MIT).
