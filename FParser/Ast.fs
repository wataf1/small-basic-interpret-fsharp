module Ast
open System
open System.Collections.Generic
open Print
type Label = string
type Identifier = string
type Index = int
type HashTable<'k,'v> = Dictionary<'k,'v>
type Arithmetic = Add | Subtract | Multiply | Divide 
    with interface IPrintable with
            member this.Formatted() =
                match this with
                | Add -> "Add"
                | Subtract -> "Subtract"
                | Multiply -> "Multiply"
                | Divide -> "Divide"
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
    with interface IPrintable with
            member this.Formatted() =
                match this with
                | Eq -> "=="
                | Ne -> "!="
                | Lt -> "<"
                | Gt -> ">"
                | Le -> "<="
                | Ge -> ">="
type Logical = And | Or
    with interface IPrintable with
            member this.Formatted() =
                match this with
                | And -> "&&"
                | Or -> "||"
type Value =
    | Bool of bool
    | Int of int
    | Double of float
    | String of string
    | Array of HashTable<Value,Value>
    with interface IPrintable with
            member this.Formatted() =
                match this with
                | Bool   x -> sprintf "(Bool %b)" x
                | Int    x -> sprintf "(Int %i)" x
                | Double x -> sprintf "(Double %A)" x
                | String x -> sprintf "(String %s)" x
                | Array  x -> 
                    let formatKvp (kv:KeyValuePair<Value,Value>) =
                        let key = (kv.Key:>IPrintable).Formatted()
                        let value = (kv.Value:>IPrintable).Formatted()
                        sprintf "%s=%s" key value
                    let s = String.Join(";",x |> Seq.map formatKvp)
                    sprintf "(Array [%s])" s

type Pattern =
    | Bind of Identifier
    | Clause of Clause
    | Tuple of Pattern list 
    with interface IPrintable with
            member this.Formatted() = 
                match this with
                | Bind x -> sprintf "(Bind %s)" x
                | Clause x -> pprint1 "(Clause %s)" x
                | Tuple xs -> sprintf "(Tuple %s)" (sjoin xs)
                    
and Clause =
    | Any
    | Is of Comparison * Value
    | Range of Value * Value
    | Pattern of Pattern
    with interface IPrintable with
            member this.Formatted() = 
                match this with
                | Any -> "Any"
                | Is(cmpr,v) -> pprint2 "Is %s %s" cmpr v
                | Range(low,high) -> pprint2 "Range (%s,%s)" low high
                | Pattern p -> pprint1 "Pattern %s" p
                
type Expr =
    | Literal of Value
    | Var of Identifier
    | GetAt of Location
    | Func of Invoke
    | Neg of Expr
    | Arithmetic of Expr*Arithmetic*Expr
    | Comparison of Expr*Comparison*Expr
    | Logical of Expr*Logical*Expr
    | NewTuple of Expr list
    with interface IPrintable with
            member this.Formatted() = 
                 match this with
                 |Literal v -> pprint1 "(Literal %s)" v
                 | Var v -> sprintf "(Var %s)" v
                 | GetAt loc -> pprint1 "(GetAt %s)" loc
                 | Func i -> pprint1 "(Func %s)" i
                 | Neg e -> pprint1 "(Neg %s)" e
                 | Arithmetic(e1,a,e2) -> pprint3 "(Arithmetic: %s %s %s)" e1 a e2
                 | Comparison(e1,c,e2) -> pprint3 "(Comparison: %s %s %s)" e1 c e2
                 | Logical(e1,l,e2) -> pprint3 "(Logical: %s %s %s)" e1 l e2
                 | NewTuple xs -> sprintf "(NewTuple %s)" (sjoin xs)
and Location = Location of Identifier*Expr list
    with interface IPrintable with
            member this.Formatted() = 
                match this with 
                | Location(i,xs) -> sprintf "Loc %s %s" i (sjoin xs)
and Invoke =
    Call of string*Expr list
    | Method of string * string * Expr list
    | PropertyGet of string*string
    with interface IPrintable with
            member this.Formatted() = 
                match this with
                | Call(name,xs) -> sprintf "Call %s %s" name (sjoin xs)
                | Method(tn,mn,xs) -> sprintf "Method %s.%s %s" tn mn (sjoin xs)
                | PropertyGet(tn,pn) -> sprintf "PropertyGet %s.%s" tn pn
type Assign = Set of Identifier * Expr
    with interface IPrintable with
            member this.Formatted() = 
                match this with
                | Set(id,e) -> sprintf "Assign %s = %s" id (pfmt e)

type ClassMember =
    | CMethod of methodname:Identifier*args:string list
    | CEndMethod
    | CProperty of Assign
type ClassDef = ClassDef of Identifier*args:string list
type Instruction =
    Assign of Assign
    | Deconstruct of Pattern * Expr
    | SetAt of Location * Expr
    | PropertySet of string * string * Expr
    | Action of Invoke
    | For of Assign * Expr * Expr
    | EndFor
    | If of Expr
    | ElseIf of Expr
    | Else
    | EndIf
    | While of Expr
    | EndWhile
    | Sub of Identifier * string list
    | EndSub
    | Label of Label
    | Goto of Label
    | Function of Identifier * string list
    | EndFunction
    | Select of Expr
    | Case of Clause list
    | EndSelect
    | Class of ClassDef
    | ClassMember of ClassMember
    | EndClass
    with interface IPrintable with
            member this.Formatted() = 
                match this with 
                | Assign a -> pfmt a          // of Assign
                | Deconstruct(p,e) -> ""      // of Pattern * Expr
                | SetAt(loc,e) ->  ""         // of Location * Expr
                | PropertySet(tn,pn,e) ->  "" // of string * string * Expr
                | Action x -> ""              // of Invoke
                | For(a,e1,e2) -> ""          // of Assign * Expr * Expr
                | EndFor -> ""
                | If e -> ""                  //of Expr
                | ElseIf e -> ""              //of Expr
                | Else -> ""
                | EndIf -> ""
                | While e -> ""               //of Expr
                | EndWhile -> ""
                | Sub (id,xs) -> ""           //of Identifier * string list
                | EndSub -> ""
                | Label x -> ""               //of Label
                | Goto x -> ""                //of Label
                | Function (id,xs) -> ""      // of Identifier * string list
                | EndFunction -> ""
                | Select e -> ""              //of Expr
                | Case xs -> ""               //of Clause list
                | EndSelect -> ""


    
type Position = {
    StartLn:int;
    StartCol:int
    EndLn:int
    EndCol:int
}