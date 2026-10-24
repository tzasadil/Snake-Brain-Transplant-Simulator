namespace SnakeGame
open System.Collections.ObjectModel

open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Documents;
open System.Windows.Input
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Navigation;
open System.Windows.Shapes
open System.Windows.Threading
open System.Windows.Interactivity
open ViewModule.FSharp
open FsXaml

(* 
module SnakeStuff = 
    
    module MyDoubleList = 
        type DoubleList<'a> = {Front : list<'a>; Back : list<'a>} 
        let AddFront a dl = {dl with Front=a :: dl.Front }
        let AddBack a dl = {dl with Back=a :: dl.Back }
        let rec PopFront dl = 
            match dl.Front with
            | [] -> match dl.Back with 
                    | [] -> None
                    | xs -> PopFront {Front= (List.rev xs); Back= []}
            | x::xs -> Some (x,xs)
        let rec PopBack dl = 
            match dl.Back with
            | [] -> match dl.Front with 
                    | [] -> None
                    | xs -> PopFront {Back= (List.rev xs); Front= []}
            | x::xs -> Some (x,xs)
        let toSeq dl= seq { 
            for a in dl.Front do yield a
            for b in List.rev dl.Back do yield b
        }

    

    [<System.Runtime.CompilerServices.IsReadOnly; Struct>]
    type snake = struct
        val body : LinkedList<Cell> 
        val direction : Directions
        val nextDirection : Directions
    end

    

    let mutable gameField

    let MusicPlayer = new MediaPlayer()

    let current:Cell
    
        
      let initialize = 
            InitializeComponent();
            Griddy.Background = BaseBrush;//Brushes.Blue;
            
            StateField = new State[columns, rows];

            for (int x = 0; x < columns; x++)
            {
                for (int y = 0; y < rows; y++)
                {
                    StateField[x, y] = State.Empty;
                }
            }
            GenerateFruit();
            NewGame(new Cell(rng.Next(0,columns), rng.Next(0, rows)));
            
        }
        

        }

        }



    
*)