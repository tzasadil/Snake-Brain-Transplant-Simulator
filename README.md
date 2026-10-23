# Snake: evolved neural network players

A Snake game in F# where a genetic algorithm evolves neural networks that learn to play it. You can play yourself, train a population, or watch the best snake of any past generation.

<!-- TODO: add a GIF of a trained snake playing. It is the first thing a visitor should see. -->

## How it works

- **Game**: 10x10 grid. The snake grows when it eats fruit and dies when it hits a wall or itself. It also starves if it goes too long without food, so a network cannot score by circling forever.
- **Senses**: the snake looks in 8 directions and measures the distance to the nearest wall, its own body, and fruit (24 inputs). The view rotates with the snake, so "left" always means the snake's left.
- **Brain**: a small multi-layer perceptron, 24 -> 18 -> 3 neurons with ReLU. The 3 outputs are turn left, go straight, and turn right.
- **Evolution**: 100 snakes play one game each in parallel. The two best survive unchanged (elitism). Fitter snakes are more likely to be picked as parents. Children mix parent weights at random, then mutate a few of them.
- **GUI**: WPF in strict MVVM. `PropertyChanged.Fody` injects `INotifyPropertyChanged`, so the view models stay plain F#.

## What I learned

- A random population either spins in place or drives into a wall. It takes several generations before snakes start chasing fruit.
- Snake gets harder as the snake grows. Unlike Tetris or Flappy Bird, speed does not set the difficulty, so the network needs a different strategy late in the game.
- Fruit on edges and in corners is the main failure. Many evolved snakes learn to patrol the edges.
- Most populations learn to turn in only one direction. Forcing both directions would need special training exercises.
- Late-game deaths come from limited vision: the snake only sees along 8 rays, not the whole board.

## Run

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

[Architecture diagram](https://drive.google.com/file/d/1WRbTG-MDYLCUJCyy3G-27LuXoD9cK_V1/view?usp=sharing)

Built at Charles University in 2019, polished in 2020 and 2021.
