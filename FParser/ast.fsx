#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\FParsec.1.0.3\\lib\\net40-client"
#r "FParsec.dll"
#r "FParsecCS.dll"
open FParsec.CharParsers
open FParsec
open System
open System.Collections.Generic
type Id = int

type Num = float

type Var = 
    Int of int
    | Bool of bool

module Store =
    let private dict = new Dictionary<Id,Var>()
    let private rand = new Random()
    let private genId() = 
        let mutable id = rand.Next()
        while dict.ContainsKey(id) do
            id<-rand.Next()
        id
    let add (v:Var) :Id=
        let id = genId()
        dict.Add(id,v)
        id
    let lookup (id:Id) =
        let b,v = dict.TryGetValue(id)
        if b then
            Some v
        else
            None
            

type TypeId = TypeId of Id 
    with 
        member this.GetValue() =
            match this with 
            | TypeId i -> 
                match Store.lookup(i) with
                | Some v -> v
                | None -> failwithf "No variable with id %i" i
        override this.ToString() =
            let id = match this with | TypeId i -> i
            match this.GetValue() with
            | Int i -> sprintf "int %i (id = %i)" i id
            | Bool b -> sprintf "int %b (id = %i)" b id
            
        static member FromInt(i:int) = Store.add (Int i) |> TypeId
        static member FromBool(b:bool) = Store.add(Bool b) |> TypeId


and TypeIds =
    TSingle of TypeId
    |  TMany of TypeId * TypeIds
and Exp = 
    Num of Num // num
    | Id of Id // id
    | Plus of Exp * Exp //Exp + Exp
    | Eq of Exp * Exp //Exp = Exp
    | IfElse of Exp*Exp*Exp // if Exp then Exp else Exp
    | Paren of Id*Exps //id ( Exps ) 
    | Let of Id*Exp*Exp //let id = Exp in Exp
and Exps =
    ESingle of Exp
    | EMany of Exp * Exps //Exp , Exps
and Fun = Fun of TypeId*TypeIds*Exp
and Funs =
    FSingle of Fun
    | FMany of Fun * Funs






        

let ws = spaces
let str s =  pstringCI s .>> ws



let ptype =
    let pint = pint32 |>> TypeId.FromInt .>> ws
    let pbool =
        let ptrue = str "true" |>> fun _ -> TypeId.FromBool(true) 
        let pfalse = str "false" |>> fun _ -> TypeId.FromBool(false) 
        ptrue <|> pfalse
    pint <|> pbool

let rtype s = run ptype s
