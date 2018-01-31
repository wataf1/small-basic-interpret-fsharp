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
        let source = File.ReadAllText(path)
        let program = Parser.parse source
        let exe = Path.GetFileNameWithoutExtension(path)
        Compiler.compileTo exe program
    with e ->
        printfn "%s" e.Message 
        Console.ReadLine()|> ignore
    0 // return an integer exit code
