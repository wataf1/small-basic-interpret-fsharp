
#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\FParsec.1.0.3\\lib\\net40-client"
#r "FParsec.dll"
#r "FParsecCS.dll"
#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\SmallBasicLib.1.2\\lib"
#r "SmallBasicLibrary.dll"
#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\FParser"
#load "Print.fs"
#load "Ast.fs"
#load "Parser.fs"
#load "Compiler.fs"
//http://trelford.com/blog/post/compiler.aspx
//https://bitbucket.org/ptrelford/smallbasiccompiler/src/eb376452d8cbf5d54c52a30243bec6f9b4b4a331/SmallBasicCompiler/Parser.fs?at=default&fileviewer=file-view-default
open FParsec
open System
open System.Reflection
open Print
open Ast
open Parser
open Compiler
open Microsoft.SmallBasic.Library

open System.IO
open System.Reflection.Emit

let read fn =
    let dir = @"c:\users\jhuber1\documents\visual studio 2017\Projects\FParser\FParser"
    let path = Path.Combine(dir,fn)
    File.ReadAllText(path)

let parsef (name:string) = parse (read name)
let pline = attempt pinstruction <|> attempt pblank 

let rinstruct s = run pinstruct s
let rinstruction s = run pinstruction s
let rline s = run pline s
let rlines s = run plines s
let rinstructpos s = run pinstructpos s


type Compiler(program:string, name:string) =

    //let 
    let parsed = parse program
    let instructions = parsed |> Array.map snd
    let exeName = sprintf "%s.exe" name
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(exeName)
    let typeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.Public)


    

    member __.Save() = assemblyBuilder.Save(exeName)


    