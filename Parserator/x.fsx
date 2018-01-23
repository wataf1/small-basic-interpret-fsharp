#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\FParsec.1.0.3\\lib\\net40-client"
#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\FParsec-Pipes.0.4.0\\lib\\net45"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "FParsec-Pipes.dll"
#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\Parserator"
open FParsec
open FParsec.Pipes
let ws = spaces


type Identifier = string
type Value =
    | Bool of bool
    | Int of int
    | Double of float
    | String of string
type Term =
    | Id of Identifier
    | Var of Value
    | TParenExpr of ParenExpr
and ParenExpr =
    | PExpr of Expr
and Expr = 
    Test of Test
    | Assign of Identifier*Expr
and Test =
    | Sum of Sum
    | Lt of Sum*Sum
and Sum =
    | Term of Term
    | Add of Sum*Term
    | Subtract of Sum*Term

type Statement =
    | If of condition:ParenExpr*thenStatement:Statement
    | IfElse of condition:ParenExpr*thenStatement:Statement*elseStatement:Statement
    | While of condition:ParenExpr*whileStatement:Statement
    | Do of doStatement:Statement*condition:ParenExpr
    | BracketStatements of Statement list 
    | SExpr of Expr
    | Empty

//https://tomassetti.me/ebnf/#examples

let pid = 
    many1Satisfy2 (fun x -> isLetter x || x = '_') (fun x -> isLetter x || isDigit x || x = '_')
let pid_ws = 
    %% +.pid -- ws -|> fun x -> x

let pbool =
    let ptrue = %% +."true" -|> fun _ -> Bool true
    let pfalse =  %% +."false" -|> fun _ -> Bool true
    ptrue <|> pfalse

let pnum =
    let tonum (nl:NumberLiteral) =
        if nl.IsInteger then nl.String |> int |> Int
        else nl.String |> float |> Double
    numberLiteral (NumberLiteralOptions.AllowFraction) "number" |>> tonum

let pstringvalue =
    between (%"\"") (%"\"") <| manySatisfy ((<>) '"')
    |>> String

let pvalue = %[pbool;pnum;pstringvalue]
// this works ^^

let pexpr, pexprRef = createParserForwardedToRef()

let pparenexpr = 
    let popen = %% ws -- '(' -- ws -|> ()
    let pclose = %% ws -- ')' -- ws -|> ()
    between popen pclose pexpr
    |>> PExpr

let pterm =
    attempt (pparenexpr |>> TParenExpr) <|> attempt (pvalue |>> Var)  <|> attempt (pid_ws |>> Id)
    //attempt (pid_ws |>> Id) <|> attempt (pparenexpr |>> TParenExpr) <|> attempt (pvalue |>> Var)
    //(pid |>> Id) >>? (pparenexpr |>> TParenExpr) >>? (pvalue |>> Var)

let psum, psumRef = createParserForwardedToRef()

let psumadd =
    %% +. psum -- ws -- '+' -- ws -- +.pterm -|> fun s t -> Add(s,t)
let psumsubtract = 
    %% +. psum -- ws  -- '-' -- ws -- +.pterm -|> fun s t -> Subtract(s,t)
do psumRef := 
    choice [
        attempt (pterm |>> Term)
        attempt psumadd
        attempt psumsubtract
    ]
   //attempt (pterm |>> Term)<|> attempt psumadd <|> attempt psumsubtract
    //attempt psumadd <|> attempt psumsubtract <|> attempt (pterm |>> Term)
    //psumadd >>? psumsubtract >>? (pterm |>> Term)

let ptest =
    let plt =
        %% +. psum -- %'<' -- +.psum -|> fun s1 s2 -> Lt(s1,s2)
    attempt (psum |>> Sum) <|> attempt plt
    //attempt plt <|> attempt (psum |>> Sum)
    //plt >>? (psum |>> Sum)
    
do pexprRef :=
    let passign =
        %% +.pid_ws -- '=' -- ws -- +.pexpr -|> fun s1 s2 -> Assign(s1,s2)
    attempt (ptest |>> Test) <|> attempt passign 
    //passign >>? (ptest |>> Test)

let pstatement, pstatementRef = createParserForwardedToRef()

let pif =
    %% "if" -- +. pparenexpr -- +. pstatement -|> fun p s -> If(p,s)

let pifelse =
    %% "if" -- +. pparenexpr -- +. pstatement -- "else" -- +. pstatement -|> fun cond s1 s2 -> IfElse(cond,s1,s2)

let pwhile =
    %% "while" -- +. pparenexpr -- +. pstatement -|> fun cond s -> While(cond,s)

let pdo =
    %% "do" -- ws -- +. pstatement -- ws -- "while" -- ws -- +. pparenexpr -- ws -- ';'
    -|> fun s cond -> Do(s,cond)

let pbracket = 
    let mstatement = many pstatement
    between (%'{') (%'}') mstatement |>> BracketStatements
    //%% '{' -- ws -- +. mstatement -- ws -- '}'

let pempty = 
    %% ws -- ';' -- ws -|> Empty

do pstatementRef :=
    [pif;pifelse;pwhile;pdo;pbracket;pempty]
    |> List.map attempt
    |> choice
    //attempt pif <|> attempt pifelse <
    //pif >>? pifelse >>? pwhile >>? pdo >>? pbracket >>? pempty
let pcomment =  pstring "//" >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'
let peol = pcomment <|> (pchar '\n')

let pstate = 
    %% ws -- +. pstatement -- peol -|> fun x -> x


let p1 =
    let popen = %% ws -- '(' -- ws -|> ()
    let pclose = %% ws -- ')' -- ws -|> ()
    let pparen = between popen pclose pvalue
    %% "if" -- +.pparen -|> fun x -> x

let r1 s = run p1 s
let rvalue s = run pvalue s
let rid s = run pid s
let rid_ws s = run pid_ws s
let rstatement s = run pstatement s
let rstate s = run pstate s
let rparen s = run pparenexpr s
let rtest s = run ptest s
    //%% 




//let ptest = %[pnum;pescaped]

//let parse s = run ptest s