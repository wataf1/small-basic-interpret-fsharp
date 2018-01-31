// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO

[<EntryPoint>]
let main argv = 
    let path = "fizz.sb"//argv.[0]
    if not (File.Exists(path)) then
        printfn "File does not exist"
        exit 0
    try 
        try
            let source = File.ReadAllText(path)
            let program = Parser.parse source
            let exe = Path.GetFileNameWithoutExtension(path)
            Compiler.compileTo exe program
            printfn "Completed compiling all %i lines" program.Length
        with e ->
            printfn "%s" e.Message 
    finally
        Console.ReadLine()|> ignore 
    0 // return an integer exit code
