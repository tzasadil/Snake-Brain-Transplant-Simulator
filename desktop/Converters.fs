namespace SnakeGame

open System.Windows.Data
open System
open System.Windows

///visible if true else collapsed
[<ValueConversion(typeof<bool>, typeof<Visibility>)>]
type BoolToVisibility() =
    interface IValueConverter with
        member _.Convert(value, targetType, parameter, culture) = 
            if value :?> bool
                then Visibility.Visible
                else Visibility.Collapsed
            |> box
        member _.ConvertBack(_,_,_,_) = failwith "undefined"

///visible when value is different from param, else collapsed
[<ValueConversion(typeof<int>, typeof<Visibility>)>]
type Neq_conv() =
    interface IValueConverter with
        member _.Convert(value, targetType, parameter, culture) = 
            if value :?> int <> (parameter :?> int)
                then Visibility.Visible
                else Visibility.Collapsed
            |> box
        member _.ConvertBack(_,_,_,_) = failwith "undefined"

///visible when value is the same as param, else collapsed
[<ValueConversion(typeof<int>, typeof<Visibility>)>]
type Eq_conv() =
    interface IValueConverter with
        member _.Convert(value, targetType, parameter, culture) = 
            if value :?> int = (parameter :?> int)
                then Visibility.Visible
                else Visibility.Collapsed
            |> box
        member _.ConvertBack(_,_,_,_) = failwith "undefined"

///boolean negation
[<ValueConversion(typeof<bool>, typeof<bool>)>]
type Not() =
    interface IValueConverter with
        member _.Convert(value, targetType, parameter, culture) =  value :?> bool |> not |> box
        member _.ConvertBack(_,_,_,_) = failwith "undefined"

/// select string from parameter based on the bool
[<ValueConversion(typeof<bool>, typeof<string>)>]
type BoolToString() =
    interface IValueConverter with
        member _.Convert(value, targetType, parameter, culture) = 
            let items =  parameter.ToString().Split '|' |> Seq.map (fun s -> s.Split '_' |> String.concat " ")
            if value :?> bool 
                then Seq.item 0 items
                else Seq.item 1 items
            :> obj
        member _.ConvertBack(_,_,_,_) = failwith "undefined"