open System  
open FsXaml  
  
type App = XAML<"App.xaml">  
  
[<EntryPoint;STAThread>]  
let main argv =
    Wpf.installBlendSupport()    
    try
        App().Run()  
     with
        | :? System.Windows.Markup.XamlParseException as e-> raise e.InnerException
    
    
