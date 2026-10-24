namespace SnakeGame


open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Threading.Tasks
open System.Threading
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Xml
open System.Xml.Serialization
open System.Runtime.Serialization
open System.Xml.Schema
open System.Windows.Media.Imaging
open System.IO
open FsXaml

open ViewModule
open ViewModule.FSharp
open ViewModule.Validation.FSharp

(* Development Documentation

Recreation of the game snake
allows playing the gase as a player, letting a selected AI play or train a new population

Major parts the program is split into:
    Snake - Holds all the information about the snake
    GameManager - Holds the information about the state of playing field
    AI - Makes turn decisions based on the game state
    Population - group of AI that compete to get better
    GUI - Allows the user to control the application and visualize the current game state

structure diagram: https://drive.google.com/file/d/1WRbTG-MDYLCUJCyy3G-27LuXoD9cK_V1/view?usp=sharing

GameManager holds information about the game state as a matrix with 'in what state each cell is'. 
Provides logic for moving the snake: 
    bump into a wall => game over
    eat fruit => grow and generate new fruit someplace else
    empty cell => move head and tail

AI is modeled as a Neural network (MLP - multi layer perceptron): 
    - as input it gets the nearest (wall, fruit, snake) cells in each of the 8 directions
    - this information is fed forward through the neural network
    - outputs probability of the movable directions => forward, left, right

The Population module provides tools to manage the population and do the genetic algorithm
Training is done through random optimization - weights of the neural network are randomly mutated
Genetic algorithm - population of 100 individuals, all play one game and based on how good they do they are selected to cross/mutate and form the next generation.
Only the better individuals get to survive/reproduce => each generation is probabilistically stronger than the last one.

GUI is written in the framework WPF fully in MVVM => Bindings are used in order to do anything.
INotifyPropertyChanged is automatically injected using library Fody to enable usage of bindings in the GUI.
Rows in the training history Datagrid can be selected to let the best AI of the generation play. Can be done on-the-fly.

--------------------------------------------------------------------------
Development notes:

New population is generated randomly and thus will always either spin in circles or go immediately run into walls. 
It takes several generations before it starts going for the fruit and evades walls.

Snake as a game gets for the ai progressively harder as it grows larger, 
while other games such as tetris and flappy bird have their difficulty set by the speed of the game, 
which makes it harder for a human player, but the ai does not care at all, so once the ai solves the game loop it can continue indifinetely.
In snake, however, when the snake gets big the AI needs different strategy 

recurring problem the AI faces seems to be collecting fruit on the edges and especialy in corners; this is often times solved by traversing the game field mainly on edges
The AI, as the training progresses, gets better in later stages but worse at the start. It could be solved by using different NN at the start and later.
In later stages, when the snake gets large, the AI struggles as it doesn't have full information of the playing field and then proceeds to bumb into itself.

The AI generally learns to use only one of right or left directions. 
This could be solved by making so sort of special execises, one for each side, to force the AI to learn to use both directions. 
However its probably too much work for too little gain.

---------------------------------------------------------------------------
User documentation:

This application lets you play the game Snake
or you can train your very own AI to play it for you!

The AI is trained in so-called population consisting of 100 individual AIs, 
which compete against each other to achieve as high score as possible. 
After competing, the best individuals are selected to create the next generation. 
So each generation is better than the last.

Controls:
    space - pause/unpause depending on whether the game is currently paused
    W - go down
    S - go up
    A - go right 
    D - go left

Window buttons:
    Pause/Play- pause/unpause the game
    New population - randomly generate a whole new population
    Train - train the current population / stop the training
    Save - save the population to a directory
    Load - load the population from a directory
    Music on / Music off - turn music on/off
    Human/bot - switches control of the snake between the player and the AI 

You can select a row in the training history to select the best AI from that generation to play the game. Works on-the-fly while an AI plays.


*)

type GenerationReport = {
    generation: int
    score : int
    ai: AI 
}

type MainWindow = XAML<"MainWindow.xaml"> 

/// view-model for the GUI
type GameViewModel() as self=
    inherit ViewModelBase()
    
    let ui : (unit -> unit)->unit= Application.Current.Dispatcher.Invoke
    let rng = Random()

    
    let MusicPlayer = MediaPlayer()
    
    do MusicPlayer.Open(new Uri("../../Music.mp3", UriKind.Relative));

    //loop music
    do MusicPlayer.MediaEnded.Add (fun _ -> 
            do MusicPlayer.Position <- TimeSpan.Zero
            do MusicPlayer.Play ())
            
    let grid  = Array2D.init Settings.columns Settings.rows <| fun _ _ ->
        new Rectangle(
            Margin = new Thickness 0.5,
            Stretch = Stretch.Fill,
            Fill = Settings.BaseBrush)
    
    /// change a color of a cell
    let draw st x y =
        let col = 
            match st with
            | CellState.Empty -> Settings.BaseBrush
            | CellState.Snake -> Settings.SnakeBrush
            | CellState.Fruit -> Settings.FruitBrush
        ui <| fun _ -> grid.[x, y].Fill <- col   // needs a dispatcher, because timer events run on a separate thread

    let timer = new System.Timers.Timer(Interval = 40.0)
    
    let game = GameManager(draw)
    
    // let the user select a directory and do an operation on it
    let ChooseDirectory cont = 
        let openFileDialog = new System.Windows.Forms.FolderBrowserDialog();
        let res = openFileDialog.ShowDialog() 
        Misc.log openFileDialog.SelectedPath 
        if res = Forms.DialogResult.OK || res = Forms.DialogResult.Yes
            then cont openFileDialog.SelectedPath 
        else do Misc.log "failed to select directory"
 
    
    /// allows the ui to update
    //https://stackoverflow.com/questions/18410532/wpf-how-to-wait-for-binding-update-to-occur-before-processing-more-code
    let Sync() = Application.Current.Dispatcher.Invoke(fun _ ->(),System.Windows.Threading.DispatcherPriority.Background)|>ignore
    
    ///toggle the game being paused
    let TogglePause() =
        do self.Paused <- not self.Paused
        do timer.Enabled <- not self.Paused
        if self.Paused  
           then MusicPlayer.Pause()
           else if self.MusicEnabled then MusicPlayer.Play()
    
    /// pause the game
    let Pause() =
        do self.Paused <- true
        do timer.Enabled <- false

    // run an operation while paused
    let PauseEverythingAnd f = 
        let isP = self.Paused
        Pause()
        Sync()
        f ()
        Sync()
        if not isP then TogglePause()  //don't unpause if the game was originally paused

    let resolveTurn = function
        | TurnOK _-> ()
        | AteFruit _ -> self.Score <- self.Score + 1
        | GameOver result-> 
            do Misc.log "hit game over"
            self.Score <-0
            game.HardRestart()

    do Settings.logging <- false


    let mutable population = Array.init Settings.PopulationSize <| fun _ -> AI <| Clone ExampleSnake.brain

    member val PlayerPlays = false with get,set
    member val TrainingGames = Settings.trainingGames with get,set 
    member val Score = 0 with get,set
    member val Paused = true with get, set 
    member val SavePath = "" with get,set
    member val Ai =  AI <| Net ExampleSnake.brain   with get, set
    member val MusicEnabled = false with get,set
    member val StopToken= new StopToken()
    member val Generation = 0 with get, set 
    member val Training = false with get, set
    member val LastIterationFinished = true with get, set
    member val dg : System.Windows.Controls.DataGrid Option = None with get, set  // reference to the datagrid showcasing training results
    member _.Columns = Settings.columns
    member _.Rows = Settings.rows
    member _.BackGround = Settings.BackGround

    member val TrainingProgressMessages = ObservableCollection<GenerationReport>()
    
    /// setup the UI
    member _.Initialize_cmd = 
        self.Factory.CommandSyncParam <| fun (gr : System.Windows.Controls.Primitives.UniformGrid) -> 
           
            for y in 0..Settings.rows-1 do
                for x in 0..Settings.columns-1 do
                    grid.[x,y] |> gr.Children.Add |> ignore       // Array2D.iter messes up the ordering in uniformgrid, so i needs to be done manualy through for-cycles
            
            do timer.Elapsed.Add <| fun _ -> // wait for the last move to finish before starting another; helps when laggy
                if not self.Paused && self.LastIterationFinished then // lastIterationFinished is not interlocked but the worst thing that can happen is a frameskip
                    self.LastIterationFinished <- false
                    if self.PlayerPlays
                        then game.Move () |> resolveTurn
                        else  self.Ai.TakeTurn game |> resolveTurn
                    self.LastIterationFinished <- true
            if self.MusicEnabled then MusicPlayer.Play()
            async{
                do! Async.Sleep 1000
                ui <| fun _ -> timer.Start()
            } |> Async.Start
    
    ///begins training of the population 
    member _.Train_cmd = self.Factory.CommandSync <| fun _->    // 1 hour ~ 3000 runs
        if self.Training 
            then do self.StopToken.StopReguested <- true    //request training to end
            else
                do self.Training <- true
                do self.StopToken.StopReguested <- false
                do self.PlayerPlays <- false
                do timer.Interval <- 40.0
                do self.dg.Value.SelectedItem <- null
                do Pause ()

                let work () = 
                    let logFunc ai gen score = async {  //report status to UI
                        ui <| fun _ ->
                            self.Generation <- self.Generation + 1
                            let item = {score=score; generation=self.Generation; ai=ai}
                            do self.TrainingProgressMessages.Insert(0,item) }|> Async.Start

                    let newPop = Population.Train self.TrainingGames population self.StopToken logFunc
                    ui <| fun _ -> //change to the new population
                        do self.Ai <- Array.last newPop
                           population <- newPop
                           self.Training <- false
                           self.dg.Value.SelectedIndex <- 0
                        //Population.Save population "Population"
                    Sync()
                    ui <| fun _ -> TogglePause()
                    Sync()

                // run on a background thread
                do Thread(work).Start()

    ///swaps between player and bot to play the game
    member public _.ChangePlayer_cmd = self.Factory.CommandSync <| fun _ ->
        do self.PlayerPlays <- not self.PlayerPlays
        if self.PlayerPlays 
        then timer.Interval <- 100.0
        else timer.Interval <- 40.0

    /// generates a new random population
    member _.GeneratePopulation_cmd = self.Factory.CommandSync <| fun _ ->
        do population <- Population.GenerateRandomly()
        self.Generation <- 0
        self.TrainingProgressMessages.Clear()

    /// loads a population from a directory
    member _.LoadPopulation_cmd = self.Factory.CommandSync <| fun _ ->
        PauseEverythingAnd (fun _ -> ChooseDirectory <| fun path -> 
            try 
                population <- Population.Load path
            with
            |_-> MessageBox.Show(sprintf "Failed to load neural network from file: %s" path) |> ignore
        )
     
    /// allows selecting the best AI from a previous generation
    member _.SelectedGenChanged_cmd = self.Factory.CommandSync <| fun _->
        if self.dg.Value.SelectedItem <> null 
        then self.Ai <- (self.dg.Value.SelectedItem :?> GenerationReport).ai
        else self.Ai <- population.[0]

    /// unselect previous generation 
    member _.Unselect_cmd = self.Factory.CommandSync<| fun _ ->
        self.dg.Value.SelectedItem <- null
        
    ///get the training-history-datagrid
    //used to get reference to the datagrid
    //needed because of MVVM
    member _.GetDg_cmd = self.Factory.CommandSyncParam <| fun (dg : System.Windows.Controls.DataGrid) ->
        self.dg  <- Some dg
        
    ///save population to a directory
    member _.SavePopulation_cmd = self.Factory.CommandSync <| fun _ ->
            PauseEverythingAnd (fun _ -> ChooseDirectory <| fun path -> 
                try 
                    Population.Save population path
                with
                |_-> MessageBox.Show(sprintf "Failed to save neural network to file: %s" path) |> ignore
            )
       
    /// toggle music on/off
    member _.ToggleMusic_cmd = self.Factory.CommandSync <| fun _ ->
        self.MusicEnabled <- not self.MusicEnabled 
        if self.MusicEnabled && not self.Paused
            then MusicPlayer.Play()
            else MusicPlayer.Stop()

    /// pause/unpause the game
    member _.Pause_cmd = self.Factory.CommandSync <| fun _ ->
       TogglePause()
    
    //movement
    member _.Left_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused
        then game.ChangeDirection Directions.Left
    member _.Right_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused
        then game.ChangeDirection Directions.Right
    member _.Down_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused
        then game.ChangeDirection Directions.Down
    member _.Up_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused 
        then game.ChangeDirection Directions.Up     
    
            