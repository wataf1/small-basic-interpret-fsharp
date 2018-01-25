
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

    let patom = 
        pliteral >>? pgetat >>? pfunc >>? (pid |>> Var)

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

let pparams = between_paren (sepBy pid (str_ws ","))

let rtest s = run (str_ws "test ") s
let rtest1 s = run (str_ws1 "test ") s
let rbool s = run pbool s
let rnum s = run pnum s
let rchar s = run pcharvalue s
let rvalue s = run pvalue s