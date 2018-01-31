module Printer
open AST
open Microsoft.FSharp.Reflection

//let getUnionFields (x:'a)  = 
//    FSharpValue.GetUnionFields(x,typeof<'a>)

////let rec toString (x:'a) =
////    match FSharpValue.GetUnionFields(x,typeof<'a>) with
////    | case, v ->
////        let name = case.Name 
////        let o = v |> Array.map toString

//let printProgram (program:(Position*Instruction) array) =
//    ()

//let printInstruction (instruction:Instruction) =
    