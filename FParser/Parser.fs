module Parser
open FParsec.CharParsers
open FParsec
open FSharp.Core
open System
open Ast
//https://bitbucket.org/ptrelford/smallbasiccompiler/src/eb376452d8cbf5d54c52a30243bec6f9b4b4a331/SmallBasicCompiler/Parser.fs?at=default&fileviewer=file-view-default
/// Skip many ' ', '\t' or '\r' chars
let ws = skipMany <| anyOf " \t\r" //spaces
/// parse string + any whitespace after
let str_ws s= pstring s .>> ws
///  parse string + at least one whitespace char after
let str_ws1 s= pstring s .>> spaces1
/// str_ws s and then return r
let str_ws_return s r = str_ws s >>% r
/// between (str_ws "(") (str_ws ")") p
let inline between_paren p = between (str_ws "(") (str_ws ")") p
/// parse true
let ptrue = stringReturn "true" true
/// parse false
let pfalse = stringReturn "false" false
//Value
/// parse bool
let pbool = ptrue <|> pfalse |>> Bool
/// parse Int or Double
let pnum = 
    let toNum (nl:NumberLiteral) =
        if nl.IsInteger then nl.String |>int |> Int
        else
            nl.String |> float |> Double
    numberLiteral (NumberLiteralOptions.AllowFraction) "number" |>> toNum
       
/// parse string
let pstringvalue =
    between (pstring "\"") (pstring "\"") <| manySatisfy (fun x -> x <> '"')
    |>> String
//Id
/// parse identifier
let pid =
    let isIdFirstChar c = isLetter c || c = '_'
    let isIdChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdFirstChar isIdChar "identifier"
/// parse identifier + ws
let pid_ws = pid .>> ws

/// parse bool, number or string to value
let pvalue = pbool <|> pnum <|> pstringvalue

//Expr
/// parse literal expression
let pliteral = pvalue |>> Literal
/// parse variable expression
let pvar = pid |>> Var

/// Parse Invoke (Call/Method/PropertyGet) =  attempt pcall <|> attempt pmemberinvoke
let pinvoke,pinvokeRef = createParserForwardedToRef()
/// Parse Location(Identifier, Expression list) = [id][indicies]. indicies = many1 (between "[" "]" pexpr)
let ploc,plocRef= createParserForwardedToRef()
//Expr
/// Parse Func expression
let pfunc = pinvoke |>> Func
/// Parse GetAt expression
let pgetat = ploc |>> GetAt
/// Parse expression of type Literal | GetAt | Func | Var 
let patom =
    choice [
        pliteral
        attempt pgetat;attempt pfunc
        attempt (pid |>> Var)
    ]
//Operators
type Assoc = Associativity
let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
/// Parse (Expr=Literal | GetAt | Func | Var) <|> between_paren Logical|Arithmetic|Comparison)
let pterm = opp.ExpressionParser
let term = (patom .>> ws) <|> between_paren pterm
opp.TermParser <- term
let addOp s precedence f = opp.AddOperator(InfixOperator(s,ws,precedence,Assoc.Left,f))
addOp "And" 1 (fun x y -> Logical(x,And,y))        // And
addOp "Or" 1 (fun x y -> Logical(x,Or,y))          // Or
addOp "+" 2 (fun x y -> Arithmetic(x,Add,y))       // +      
addOp "-" 2 (fun x y -> Arithmetic(x,Subtract,y))  // -     
addOp "*" 3 (fun x y -> Arithmetic(x,Multiply,y))  // *     
addOp "/" 3 (fun x y -> Arithmetic(x,Divide,y))    // /   
opp.AddOperator(PrefixOperator("-",ws,2,true,Neg)) // -1      
let comparisons = ["=",Eq; "<>",Ne; "<=",Le; ">=",Ge; "<",Lt; ">",Gt]
let addComp (s:string,op:Comparison) = opp.AddOperator(InfixOperator(s,ws,2,Assoc.Left, fun x y -> Comparison(x,op,y)))
comparisons |> List.iter addComp                   // =, <>, <=, >=, <, >
/// Parse NewTuple = ( op <|> new tuple , op <|> new tuple 
let pnewtuple, pnewtupleRef = createParserForwardedToRef()
/// Parse contruct -  attempt pterm <|> attempt pnewtuple. 
let pconstruct = attempt pterm <|> attempt pnewtuple
do pnewtupleRef :=
   between_paren (sepBy1 pconstruct (str_ws ","))
    |>> NewTuple
/// pexpr = pconstruct
let pexpr = pconstruct
/// parse member/method. [type].[method]
let pmember = pipe3 pid_ws (pchar '.') pid_ws (fun tn _ mn -> tn,mn)
/// parse arguments. ([expr],[expr]..)
let pargs = between_paren (sepBy pexpr (str_ws ","))
/// parse method or property get... [type].[method]([optional args]) - pipe2 pmember (opt pargs) 
let pmemberinvoke =
    let invoke (tn,mn) args =
         match args with
                | Some args -> Method(tn,mn,args)
                | None -> PropertyGet(tn,mn)
    pipe2 pmember (opt pargs) invoke
/// parse Call = identifier(args) = pid_ws .>>. pargs
let pcall = pid_ws .>>. pargs |>> Call
do pinvokeRef := attempt pcall <|> attempt pmemberinvoke
/// Parse Action = pinvoke = (attempt pcall <|> attempt pmemberinvoke)
let paction = pinvoke |>> Action
/// Parse set = [id] = [expr] 
let pset = pipe3 pid_ws (str_ws "=") pexpr (fun id _ e -> Set(id,e))
///Parse assign = pset = [id] = [expr] 
let passign = pset |>> Assign
/// Parse property set = [type].[property] = [expr]
let ppropertyset = pipe3 pmember (str_ws "=") pexpr (fun (tn,pn) _ e -> PropertySet(tn,pn,e))
//Location
/// Parse index = [expr] = between "[" "]" pexpr
let pindex = between (str_ws "[") (str_ws "]") pexpr
/// Parse many1 index = many1 (between "[" "]" pexpr)
let pindices = many1 pindex
/// [id][indicies]. indicies = many1 (between "[" "]" pexpr)
do plocRef := pipe2 pid_ws pindices (fun id xs -> Location(id,xs))
//SetAt
/// [location] = [expr]. location = [id][indicies]. indicies = many1 (between "[" "]" pexpr)
let psetat = pipe3 ploc (str_ws "=") pexpr (fun loc _ e -> SetAt(loc,e))



//For
/// Parse For [from] To [to] (optional Step [step])
let pfor =
    let pfrom = pstring "For" >>. spaces1 >>. pset
    let pto = pstring "To" >>. spaces1 >>. pexpr
    let pstep = pstring "Step" >>. spaces1 >>. pexpr
    let toStep = function None -> Literal(Int(1)) | Some s -> s
    pipe3 pfrom pto (opt pstep) (fun f t s -> For(f,t,toStep s))
/// Parse EndFor
let pendfor = str_ws_return "EndFor" EndFor
/// Parse While [expr]
let pwhile = str_ws1 "While" >>. pexpr |>> While
/// Parse EndWhile
let pendwhile = str_ws_return "EndWhile" EndWhile
//If
/// Parse If [expr] Then
let pif = str_ws1 "If" >>. pexpr .>> str_ws "Then" |>> If
/// Parse ElseIf [expr] Then
let pelseif = str_ws1 "ElseIf" >>. pexpr .>> str_ws "Then" |>> ElseIf
/// Parse Else
let pelse = str_ws_return "Else" Else
/// Parse EndIf
let pendif = str_ws_return "EndIf" EndIf
//Sub
/// Parse ([id],[id]..) between_paren (sepBy pid_ws (str_ws ","))
let pparams = between_paren (sepBy pid_ws (str_ws ","))
/// Parse [id] [optional params]
let pmethod = 
    pid_ws .>>. opt pparams
    |>> fun (name,pars) -> name, pars |> function None -> [] | Some v -> v//Option.defaultValue [] 
/// Parse Sub [method] = Sub [id]([optional params])    
let psub = str_ws1 "Sub" >>. pmethod |>> Sub
/// Parse EndSub
let pendsub = str_ws_return "EndSub" EndSub
//Label
/// Parse [id]:
let plabel = pid_ws .>> str_ws ":" |>> Label
/// Parse goto [id]
let pgoto = str_ws1 "Goto" >>. pid |>> Goto
//Function
/// Parse Function [method]
let pfunction = str_ws1 "Function" >>. pmethod |>> Function
/// Parse EndFunction
let pendfunction = str_ws_return "EndFunction" EndFunction
//Select
/// Parse Select Case [expr]
let pselect = str_ws1 "Select" >>. str_ws1 "Case" >>. pexpr |>> Select
let ptuple, ptupleRef = createParserForwardedToRef()
/// Parse [value] To [value]
let prange = pvalue .>> ws .>> str_ws1 "To" .>>. pvalue |>> Range
/// Parse comparison
let pcomparison = 
    choice [
        for s,op in comparisons -> str_ws s >>% op
    ]
/// Parse 'Is [comparison] [value]'
let pis = str_ws1 "Is" >>. pcomparison .>>. pvalue |>> Is
/// Parse pvalue |>> Is(Eq,[value])
let pisequal = pvalue |>> fun x -> Is(Eq,x)
/// Parse 'Else'
let pany = str_ws "Else" >>% Any
/// Parse clause = Range|Is|IsEqual|Any|Tuple (Pattern list)
let pclause =
    attempt prange <|> attempt pis <|> attempt pisequal <|> 
    attempt pany <|> attempt (ptuple |>> Pattern)
/// Parse Case ([clause], ...)
let pcase = str_ws1 "Case" >>. sepBy pclause (str_ws ",") |>> Case
/// Parse EndSelect
let pendselect = str_ws_return "EndSelect" EndSelect

/// Parse bind pattern = [id] 
let pbind = pid_ws |>> Bind
/// Parse pattern = attempt ptuple <|> attempt pclause <|> attempt pbind
let ppattern =
    attempt ptuple <|> (attempt pclause |>> Clause) <|> attempt pbind
/// Parse tuple = ([pattern],...) = between_paren (sepBy ppattern (str_ws ","))
do ptupleRef := between_paren (sepBy ppattern (str_ws ",")) |>> Tuple
/// Parse tuple deconstruct = pipe3 ptuple (str_ws "=") pexpr
let pdeconstruct = pipe3 ptuple (str_ws "=") pexpr (fun p _ e -> Deconstruct(p,e))
/// Parse Instruction = For|EndFor|While|If|ElseIf|Else|EndIf|Select|Case|EndSelect|Sub|EndSub|Function|EndFunction|PropertySet|
/// Assign|SetAt|Deconstruct|Action|Label|Goto
let pinstruct =
    [
        pfor;pendfor
        pwhile;pendwhile
        pif;pelseif;pelse;pendif
        pselect; pcase; pendselect
        psub; pendsub
        pfunction; pendfunction
        ppropertyset; passign; psetat; pdeconstruct
        paction
        plabel; pgoto
    ] 
    |> List.map attempt
    |> choice

type FPosition =FParsec.Position

/// Convert start + end FPosition to Position. StartLn=p1.Line. StartCol=p1.Column. EndLn = p2.Line. EndCol = p2.Column
let toPosition (p1:FPosition) (p2:FPosition) = 
    {StartLn=int p1.Line;StartCol=int p1.Column;EndLn=int p2.Line;EndCol=int p2.Column}
/// Either Position+Instruction or Blank
type Line = Blank | Instruction of Position*Instruction
/// Parse comment. Skip all characters (until \n) after a ' 
let pcomment =  pchar '\'' >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'
//pchar '\'' >>. skipRestOfLine false >>. newline
/// Parse end of line char = pcomment or pchar '\n'
let peol = pcomment <|> (pchar '\n')
/// Parse instruction and return its position.
let pinstructpos =
    pipe3 getPosition pinstruct getPosition (fun p1 i p2 -> toPosition p1 p2, i)
/// Parse non-blank line (instruction) = ws >>. pinstructpos .>> peol
let pinstruction = ws >>. pinstructpos .>> peol |>> Instruction
/// Parse blank line
let pblank = ws >>. peol >>% Blank
/// Parse a string into line = many (attempt pinstruction <|> attempt pblank) .>> eof
let plines = many (attempt pinstruction <|> attempt pblank) .>> eof
//run
let parse (program:string) =    
    match run plines program with
    | Success(result, _, _)   -> 
        result 
        |> List.choose (function Instruction(pos,i) -> Some(pos,i) | Blank -> None) 
        |> List.toArray
    | FParsec.CharParsers.Failure(msg,e,s)->failwith msg