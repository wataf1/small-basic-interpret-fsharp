
#if INTERACTIVE
#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\FParsec\\lib\\net40-client"
#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\FParsec-Pipes\\lib\\net45"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "FParsec-Pipes.dll"
#load "AST.fs"
#else
module Parser
#endif 
open FParsec
open FParsec.CharParsers
open FParsec.Pipes
open AST

let (<|>%) p1 p2 = attempt p1 <|> attempt p2

let pws = anyOf " \t\r"
let ws = skipMany pws
let ws1 = skipMany1 pws

let str_ws (s:string) = pstring s .>> ws
let str_ws1 (s:string) = pstring s .>> ws1
let str_ws_return (s:string) r= str_ws s >>% r
let inline between_paren p = between (str_ws "(") (str_ws ")") p
let inline between_str opens closes p = between (str_ws opens) (str_ws closes) p

module PVal =

    let pbool =
        let ptrue = stringCIReturn "true" true
        let pfalse = stringCIReturn "false" false
        ptrue <|> pfalse |>> Bool

    let pnum =
        let parseHex s = System.Int64.Parse(s, System.Globalization.NumberStyles.HexNumber)
        //let hexToString (i:int64) = i.ToString("X")
        let tonum (nl:NumberLiteral) =
            if nl.IsInteger then nl.String |> int |> Int
            elif nl.IsHexadecimal then nl.String |> parseHex|> Hex
            else
                nl.String |> float |> Double

        let opts = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowHexadecimal
        numberLiteral opts "number" |>> tonum

    let pstringvalue = between_str "\"" "\"" <| manySatisfy ((<>) '"') |>> String

    let pid =
        let first c = isLetter c || c = '_'
        let rest c = isLetter c || c = '_' || isDigit c
        many1Satisfy2L first rest "identifier"

    let pid_ws =
        pid .>> ws

    let pcharvalue =
        let normalEscaped = 
            (anyOf "\"\\/bfnrt") |>> 
                function
                | 'b' -> '\b'
                | 'f' -> '\u000C'
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 't' -> '\t'
                | c   -> c // every other char is mapped to itself
        let unicodeEscaped = 
            let hex2int c i = ((int c &&& 15) + (int c >>> 6)*9)*i
            //let pdigit = many isDigit
            %"u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3 4096) + (hex2int h2 256) + (hex2int h1 16) + (hex2int h0 1)
                |> char)
        let escaped = %"\\" >>. (normalEscaped <|> unicodeEscaped)
        between_str "'" "'" escaped |>> Char

    let pvalue = pbool <|> pnum <|> pstringvalue <|> pcharvalue
open PVal

module PExpr =
    let pliteral = pvalue |>> Literal
    let pvar = pid |>> Var

    let pinvoke,pinvokeRef = createParserForwardedToRef()
    let ploc,plocRef = createParserForwardedToRef()

    let pfunc = pinvoke |>> Func
    let pgetat = ploc |>> GetAt

    let patom =         //pliteral >>? pgetat >>? pfunc >>? (pid |>> Var)
        choice [
            pliteral
            attempt pgetat;attempt pfunc
            attempt (pid |>> Var)
        ]

    type Assc = FParsec.Associativity
    let opp = new OperatorPrecedenceParser<Expr,unit,unit>()

    /// Parse Expr cases: Neg|Arithmetic|Comparison|Logical
    let pterm = opp.ExpressionParser
    let term = (patom .>> ws) <|> between_paren pterm
    opp.TermParser <- term
    let addOp s p f = opp.AddOperator(InfixOperator(s,ws,p,Assc.Left,f))
    addOp "&&" 1 (fun x y -> Logical(x,And,y))        // And
    addOp "||" 1 (fun x y -> Logical(x,Or,y))          // Or
    addOp "+" 2 (fun x y -> Arithmetic(x,Add,y))       // +      
    addOp "-" 2 (fun x y -> Arithmetic(x,Subtract,y))  // -     
    addOp "*" 3 (fun x y -> Arithmetic(x,Multiply,y))  // *     
    addOp "/" 3 (fun x y -> Arithmetic(x,Divide,y))    // /   
    addOp "^" 3 (fun x y -> Arithmetic(x,Pow,y))       // ^   
    addOp "%" 3 (fun x y -> Arithmetic(x,Mod,y))       // %   
    opp.AddOperator(PrefixOperator("-",ws,2,true,Neg))
    let comparisons = ["=",Eq; "<>",Ne; "<=",Le; ">=",Ge; "<",Lt; ">",Gt]
    let addComp (s:string,op:Comparison) = opp.AddOperator(InfixOperator(s,ws,2,Assc.Left, fun x y -> Comparison(x,op,y)))
    comparisons |> List.iter addComp  

    let pnewtuple, pnewtupleRef = createParserForwardedToRef()
    /// Parse Expr cases: NewTuple|Neg|Arithmetic|Comparison|Logical
    let pconstruct = attempt pterm <|> attempt pnewtuple
    do pnewtupleRef :=
        between_paren (sepBy1 pconstruct (str_ws ",")) |>> NewTuple
    let pexpr = pconstruct

    let pmember = pipe3 pid_ws (%'.') pid_ws (fun typeName _ methodName -> typeName,methodName)

    let pargs = between_paren (sepBy pexpr (str_ws ","))

    let pmemberinvoke =
        let invoke (typeName,methodName) = function
            | Some(xs) -> Method(typeName,methodName,xs)
            | None -> PropertyGet(typeName,methodName)
        pipe2 pmember (opt pargs) invoke

    let pcall = pid_ws .>>. pargs |>> Call



    do pinvokeRef := pcall <|>% pmemberinvoke
        //attempt pcall <|> attempt pmemberinvoke
    let paction = pinvoke |>> Action
    //attempt pcall <|> attempt pmemberinvoke
open PExpr

module PAssign =
    let pset = pipe3 pid_ws (str_ws "=") pexpr (fun id _ e -> Set(id,e))
    let passign = pset |>> Assign
    let ppropertyset = pipe3 pmember (str_ws "=") pexpr (fun (typeName,methodName) _ e -> PropertySet(typeName,methodName,e))
    let pindex = between_str "[" "]" pexpr
    let pindices = many1 pindex
    do plocRef := 
        pipe2 pid_ws pindices (fun id idx -> Location(id,idx))
    let psetat = pipe3 ploc (str_ws "=") pexpr (fun l _ e -> SetAt(l,e)) 
open PAssign

module PBranch =
//Might be messed up
    let pfor =
        let step = str_ws1 "Step" >>. pexpr
        let toStep = function Some x -> x | None -> Literal(Int(1))
        %% str_ws1 "For" -- +.pset -- str_ws1 "To" -- +.pexpr -- +.(opt step) -|> fun a e o -> For(a,e,toStep o)

    let pendfor = str_ws_return "EndFor" EndFor
    let pwhile = str_ws1 "While" >>. pexpr |>> While
    let pendwhile = str_ws_return "EndWhile" EndWhile
    let pif = str_ws1 "If" >>. pexpr .>> str_ws "Then" |>> If
    let pelseif = str_ws1 "ElseIf" >>. pexpr .>> str_ws "Then" |>> ElseIf
    let pelse = str_ws_return "Else" Else
    let pendif = str_ws_return "EndIf" EndIf
open PBranch
module PMethod =
    let pparams = between_paren (sepBy pid (str_ws ","))

    let pmethod =
        pid_ws .>>. opt pparams
        |>> fun (name,pars) -> name, defaultArg pars []

    let psub = str_ws1 "Sub" >>. pmethod |>> Sub
    let pendsub = str_ws_return "EndSub" EndSub

    let plabel = pid_ws .>> str_ws ":" |>> Label

    let pgoto = str_ws1 "Goto" >>. pid |>> Goto

    let pfunction = str_ws1 "Function" >>. pmethod |>> Function
    let pendfunction = str_ws_return "EndFunction" EndFunction
open PMethod

module PSelect =
    let pselect = str_ws1 "Select" >>. str_ws1 "Case" >>. pexpr |>> Select
    let ptuple, ptupleRef = createParserForwardedToRef()

    let prange = pvalue .>> ws .>> str_ws "To" .>>. pvalue |>> Range
    let pcomparison =
        choice [
            for s,op in comparisons -> str_ws s >>% op
        ]

    let pis = str_ws1 "Is" >>. pcomparison .>>. pvalue |>> Is
    let pisequal = pvalue |>> fun x -> Is(Eq,x)
    let pany = str_ws_return "Else" Any
    let pclause = prange <|>% pis <|>% pisequal <|>% pany <|>% (ptuple |>> Pattern)

    let pcase = str_ws1 "Case" >>. sepBy pclause (str_ws ",") |>> Case
    let pendselect = str_ws_return "EndSelect" EndSelect
    let pbind = pid_ws |>> Bind
    let ppattern = ptuple <|>% (pclause |>> Clause) <|>% pbind
    do ptupleRef := between_paren (sepBy ppattern (str_ws ",")) |>> Tuple
    let pdeconstruct = pipe3 ptuple (str_ws "=") pexpr (fun p _ e -> Deconstruct(p,e))
open PSelect

let pfield = 
    pipe4 (str_ws1 "Field") pid_ws (str_ws "=") pexpr (fun _ id _ e -> Field(id,e))

let pstartfield = (str_ws1 "Field") >>. pid_ws |>> StartField
let pendfield = str_ws_return "EndField" EndField

let pfieldvalue =
    (str_ws "Value") >>. (str_ws "=") >>. pexpr |>> FieldValue

//let pfield = pfieldln <|>% 

let pinstruct =
    [
        pfor;pendfor
        pwhile;pendwhile
        pif;pelseif;pelse;pendif
        pselect; pcase; pendselect;
        psub; pendsub
        pfunction; pendfunction
        pfield; pstartfield; pendfield; pfieldvalue
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

type Line = Blank | Instruction of Position*Instruction
let pcomment =  pchar '\'' >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'
let peol = pcomment <|> (pchar '\n')
let pinstructpos =
    pipe3 getPosition pinstruct getPosition (fun p1 i p2 -> toPosition p1 p2, i)
let pinstruction = ws >>. pinstructpos .>> peol |>> Instruction
let pblank = ws >>. peol >>% Blank
let plines = many (attempt pinstruction <|> attempt pblank) .>> eof
//run
let parse (program:string) =    
    match run plines program with
    | Success(result, _, _)   -> 
        result 
        |> List.choose (function Instruction(pos,i) -> Some(pos,i) | Blank -> None) 
        |> List.toArray
    | FParsec.CharParsers.Failure(msg,e,s)->failwith msg
module Run =
    let rtest s = run (str_ws "test ") s
    let rtest1 s = run (str_ws1 "test ") s
    let rbool s = run pbool s
    let rnum s = run pnum s
    let rchar s = run pcharvalue s
    let rvalue s = run pvalue s
open Run
open System.IO

let rinvoke s = run pinvoke s

let parsef (name:string) =
    let path = Path.Combine(@"C:\Users\jhuber1\Documents\Visual Studio 2017\Projects\FParser\Parserator",name)
    let program = File.ReadAllText(path)
    parse program