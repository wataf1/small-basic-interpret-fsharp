module AST
open System
open System.Collections.Generic
type Label = string
type Identifier = string
type Index = int
type HashTable<'k,'v> = Dictionary<'k,'v>
type Arithmetic = Add | Subtract | Multiply | Divide | Pow | Mod

type Comparison = Eq | Ne | Lt | Gt | Le | Ge

type Logical = And | Or

type Value =
    | Bool of bool
    | Int of int
    | Double of float
    | Hex of int64
    | String of string
    | Char of char
    | Array of HashTable<Value,Value>

type Pattern =
    | Bind of Identifier
    | Clause of Clause
    | Tuple of Pattern list 
           
and Clause =
    | Any
    | Is of Comparison * Value
    | Range of Value * Value
    | Pattern of Pattern
                
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

and Location = Location of Identifier*Expr list

and Invoke =
    Call of string*Expr list
    | Method of string * string * Expr list
    | PropertyGet of string*string

//type Field

type Assign = Set of Identifier * Expr

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
    | Field of Identifier*Expr
    | StartField of Identifier
    | EndField
    | FieldValue of Expr

    
type Position = {
    StartLn:int;
    StartCol:int
    EndLn:int
    EndCol:int
}