module Compiler
open Ast
open Microsoft.SmallBasic.Library
open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic

let inline notimpl() = raise <| NotImplementedException()
let tprimitive = typeof<Primitive>

/// Generated fields for set operations defined in instructions
let generateFields (typeBuilder:TypeBuilder) (instructions:Instruction array) =
    let generateField name = typeBuilder.DefineField(name,typeof<Primitive>, FieldAttributes.Static)
    let (|Bound|) pattern =
        let rec bound = function
            | Bind name -> [name]
            | Clause _ -> []
            | Tuple xs -> [for x in xs do yield! bound x]
        bound pattern
    
    [for instruction in instructions do
        match instruction with
        | Assign(Set(name,_)) -> yield name
        | SetAt(Location(name,_),_) -> yield name
        | Deconstruct(Bound names,_) -> yield! names
        | For(Set(name,_),_,_) -> yield name
        | _ -> ()]
    |> List.distinct
    |> List.map (fun x -> x, generateField x)
    |> dict
/// Generate methods for named subroutines
let generateMethods (typeBuilder:TypeBuilder) (instructions:Instruction array) =
    let chooseMethod = function
        | Sub(name,ps) -> Some(name,typeof<Void>,ps)
        | Function(name,ps) -> Some(name,typeof<Primitive>,ps)
        | _ -> None
    let generateMethod name returnType (ps:string list)=
        let mi =
            typeBuilder.DefineMethod(
                name, 
                MethodAttributes.Static ||| MethodAttributes.Public,
                returnType, 
                ps |> List.map (fun _ ->typeof<Primitive>) |> Array.ofList)
        ps |> List.iteri (fun i x -> mi.DefineParameter(i+1, ParameterAttributes.None, x) |> ignore) 
        mi
    //instructions
    //|> Array.choose chooseMethod
    [for instruction in instructions do
        match instruction with
        | Sub(name, ps) -> yield name, typeof<Void>, ps
        | Function(name, ps) -> yield name, typeof<Primitive>, ps
        | _ -> ()
    ]
    |> List.map (fun (name,ty,ps) -> name, (generateMethod name ty ps,ps))
    |> dict

type ILGenerator with
    /// Emit new Primitive created using 1 parameter constructor of type specified
    member il.EmitNewPrimitive(t:Type) =
        let ci = typeof<Primitive>.GetConstructor([|t|])
        il.Emit(OpCodes.Newobj,ci)

let emitInstructions (mainIL:ILGenerator) doc 
        (methods:IDictionary<Identifier,MethodBuilder*string list>)
        (fields:IDictionary<Identifier,FieldBuilder>) (program:(Position*Instruction) array) =
    let fieldLookup name = fields.[name]
    /// IL generator for current method
    let methodIL = ref mainIL
    /// Name of current method
    let methodName = ref "Main"
    /// For for and while statements
    let loopStack = Stack<Label*Label>()
    /// For if statements
    let ifStack = Stack<Label*Label>()
    /// For case statements
    let caseStack = Stack<(Label*Label) option>()
    /// labels for goto statements
    let labels = Dictionary<string, Label>()
    /// Gets an existing label or creates a new label
    let obtainLabel (il:ILGenerator) name =
        match labels.TryGetValue(name) with
        | true, label -> label
        | false, _ ->
            let label = il.DefineLabel()
            labels.Add(name,label)
            label
    let getLibraryTypeName name = sprintf "Microsoft.SmallBasic.Library.%s, SmallBasicLibrary" name
    /// Emit new Primitive created using 1 parameter constructor of type specified
    let emitNewPrimitive (il:ILGenerator) t =
        let ci = typeof<Primitive>.GetConstructor([|t|])
        il.Emit(OpCodes.Newobj,ci)
    /// Emit new Primitive(bool|int|float|string).
    let emitLiteral (il:ILGenerator) = function
        | Bool(true) -> 
            il.Emit(OpCodes.Ldc_I4_1) // pushes the value of 1 onto the evaluation stack as an int32
            il.EmitNewPrimitive typeof<bool>   
        | Bool(false) -> 
            il.Emit(OpCodes.Ldc_I4_0) // pushes the value of 0 onto the evaluation stack as an int32
            il.EmitNewPrimitive typeof<bool>
        | Int i ->  
            il.Emit(OpCodes.Ldc_I4,i)
            il.EmitNewPrimitive typeof<int>
        | Double d ->  
            il.Emit(OpCodes.Ldc_R8,d) // pushes supplied value of type float64 onto stack
            il.EmitNewPrimitive typeof<float>
        | String s ->
            il.Emit(OpCodes.Ldstr,s)
            il.EmitNewPrimitive typeof<string>
        | Array _ -> raise <| NotImplementedException()
    /// Emit new SmallBasicCallback(null, method specified) 1) load null 2) pointer to method w/ specified name 3) Create new SmallBasicCall(object,IntPtr)
    let emitNewCallback (il:ILGenerator) name =
        il.Emit(OpCodes.Ldnull) 
        il.Emit(OpCodes.Ldftn, fst methods.[name])//:>MethodInfo) //pushes an unmanged pointer to the native code, implementing a method on the the stack
        let cb = typeof<SmallBasicCallback>
        let ci = cb.GetConstructor([|typeof<obj>; typeof<IntPtr>|])
        il.Emit(OpCodes.Newobj,ci)
    /// Emit load arg at index specified onto stack
    let emitLdArg (il:ILGenerator) = function
        | 0 -> il.Emit(OpCodes.Ldarg_0)
        | 1 -> il.Emit(OpCodes.Ldarg_1)
        | 2 -> il.Emit(OpCodes.Ldarg_2)
        | 3 -> il.Emit(OpCodes.Ldarg_3)
        | x -> il.Emit(OpCodes.Ldarg,x)
    /// Emit expression
    let rec emitExpression (il:ILGenerator) = function
        | Literal x -> emitLiteral il x
        | Var name -> 
            let paramIndex =
                match methods.TryGetValue(!methodName) with
                | true, (_,ps) -> ps |> List.tryFindIndex ((=) name)
                | false, _ -> None
            match paramIndex with
            | Some i -> emitLdArg il i
            | None -> 
                match fields.TryGetValue(name) with
                | true, field -> il.Emit(OpCodes.Ldsfld,field)
                | false, _ -> emitNewCallback il name
        | GetAt(Location(name,indices)) ->
            let emitGetArrayValue (index:Expr) = emitPrimitiveCall il index "GetArrayValue" 
            //emitExpression il index; let mi = typeof<Primitive>.GetMethod("GetArrayValue"); il.EmitCall(OpCodes.Call,mi,null)
            il.Emit(OpCodes.Ldsfld, fieldLookup name)
            for index in indices do
                emitExpression il index
                let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                il.EmitCall(OpCodes.Call, mi, null)
            //indicies |> List.iter emitGetArrayValue
        | Func invoke  -> emitInvoke il invoke
        | Neg e -> //emitPrimitiveCall il e "op_UnaryNegation" //emitExpression il e; let mi = typeof<Primitive>.GetMethod("op_UnaryNegation"); il.EmitCall(OpCodes.Call,mi,null)
            emitExpression il e 
            let mi = typeof<Primitive>.GetMethod("op_UnaryNegation")
            il.EmitCall(OpCodes.Call, mi, null)
        | Arithmetic(lhs,Add,rhs) -> emitOp il lhs rhs "op_Addition"
        | Arithmetic(lhs,Subtract,rhs) -> emitOp il lhs rhs "op_Subtraction"
        | Arithmetic(lhs,Multiply,rhs) -> emitOp il lhs rhs "op_Multiply"
        | Arithmetic(lhs,Divide,rhs) -> emitOp il lhs rhs "op_Division"
        | Comparison(lhs,op,rhs) -> op |> toOp |> emitOp il lhs rhs
        | Logical(lhs,And,rhs) -> emitOp il lhs rhs "op_And"
        | Logical(lhs,Or,rhs) -> emitOp il lhs rhs "op_Or"
        | NewTuple xs -> newTuple il xs
    /// Declare a new local primitive. For each expr in list (index i) emit:
    /// 1) Expr
    /// 2) Load local var @ index localBuilder.LocalIndex onto stack
    /// 3) Call to Primitve.SetArrayValue(i)
    /// 4) Pop current value from stack, store it @ localBuilder.LocalIndex 
    /// Lastly, emit a load local var at localBuilder.LocalIndex
    and newTuple (il:ILGenerator) xs =
        let array = il.DeclareLocal(typeof<Primitive>)
        xs |> List.iteri (fun i x ->
            emitExpression il x
            il.Emit(OpCodes.Ldloc, array.LocalIndex)
            //emitPrimitiveCall il (Literal(Int(i))) "SetArrayValue"
            emitExpression il (Literal(Int(i)))
            let mi = typeof<Primitive>.GetMethod("SetArrayValue")
            il.EmitCall(OpCodes.Call, mi, null)
            il.Emit(OpCodes.Stloc, array.LocalIndex)
            )
        il.Emit(OpCodes.Ldloc, array.LocalIndex)
    /// Emit expression expr. Get method Primitive.[name] and emit call
    and emitPrimitiveCall (il:ILGenerator) expr name  =
        emitExpression il expr
        let mi = typeof<Primitive>.GetMethod(name)
        il.EmitCall(OpCodes.Call,mi,null)
    /// Get (Primitive.)method name for given op
    and toOp = function
        | Eq -> "op_Equality" | Ne -> "op_Inequality"
        | Lt -> "op_LessThan" | Gt -> "op_GreaterThan"
        | Le -> "op_LessThanOrEqual" | Ge -> "op_GreaterThanOrEqual"
    /// 1) Emit lhs expression 2) Emit rhs expression 3) Emit call to Primitive.op
    and emitOp (il:ILGenerator) lhs rhs op =
        emitExpression il rhs
        emitExpression il lhs
        let mi = typeof<Primitive>.GetMethod(op)
        il.EmitCall(OpCodes.Call,mi,null)
    /// Emits call to method or property specified.
    and emitInvoke (il:ILGenerator) = function
        | Call(name,args) ->  
            emitArgs il args
            let mi,_ = methods.[name]
            il.EmitCall(OpCodes.Call,mi,[||])
        | Method(typeName, methodName, args) -> 
            emitArgs il args
            let types = [|for _ in args -> typeof<Primitive>|]
            let fullTypeName = getLibraryTypeName typeName
            let mi = Type.GetType(fullTypeName).GetMethod(methodName, types)
            il.EmitCall(OpCodes.Call, mi, null)
        | PropertyGet(typeName, propName) -> 
            let typeName = getLibraryTypeName typeName
            let pi = Type.GetType(typeName).GetProperty(propName)
            il.EmitCall(OpCodes.Call, pi.GetGetMethod(), null)
    /// For each arg, emit expression 
    and emitArgs (il:ILGenerator) args = //args |> List.iter (emitExpression il)
        for arg in args do emitExpression il arg
    /// Emit expression e from (Set(name,e)). Emit load static field of name)
    let emitSet (il:ILGenerator) (Set(name,e)) =
        emitExpression il e
        il.Emit(OpCodes.Stsfld, fieldLookup name)
    /// Emit expression. (Location(name,indices),lasti=indices.Length-1) 
    /// For li = lasti to 0, load static field name and {For i = 0 to (lasti - 1), emitPrimitive call to indices[i] GetArrayValue}
    /// Emit primitive call to indices[li] "SetArrayValue"
    let emitSetAt (il:ILGenerator) (Location(name,indices),e) =
        emitExpression il e
        let lastIndex = indices.Length - 1
        for lastIndex = indices.Length - 1 downto 0 do
            il.Emit(OpCodes.Ldsfld, fieldLookup name)
            for index = 0 to lastIndex - 1 do
                emitExpression il indices.[index]
                let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                il.EmitCall(OpCodes.Call, mi, null)
            emitExpression il indices.[lastIndex]
            let mi = typeof<Primitive>.GetMethod("SetArrayValue")
            il.EmitCall(OpCodes.Call, mi, null)
        il.Emit(OpCodes.Stsfld, fieldLookup name)
        //for lastIndex = indices.Length - 1 downto 0 do
            //il.Emit(OpCodes.Stsfld, fieldLookup name)
            //for index = 0 to lastIndex - 1 do
                //emitPrimitiveCall il indices.[index] "GetArrayValue"
            //emitPrimitiveCall il indices.[lastIndex] "SetArrayValue"
    /// Emit the IL to set [typeName].[name] to expression
    let emitPropertySet (il:ILGenerator) typeName name e =
        emitExpression il e
        let typeName = getLibraryTypeName typeName
        let ty = Type.GetType(typeName)
        let pi = ty.GetProperty(name)
        if isNull pi then
            let ei = ty.GetEvent(name)
            il.EmitCall(OpCodes.Call, ei.GetAddMethod(), null)
        else
            il.EmitCall(OpCodes.Call, pi.GetSetMethod(), null)
    /// Emit call to Primitive.ConvertToBoolean
    let emitConvertToBool (il:ILGenerator) =
        let mi = typeof<Primitive>.GetMethod("ConvertToBoolean")
        il.EmitCall(OpCodes.Call,mi,null)
    /// Emits the IL for a given instruction
    let emitInstruction (il:ILGenerator) = function
        | Assign set -> emitSet il set
        | Deconstruct(pattern,e) -> 
            emitExpression il e
            let rec deconstruct = function
                | Bind("_") -> il.Emit(OpCodes.Pop)
                | Bind(name) -> il.Emit(OpCodes.Stsfld, fieldLookup name)
                | Clause _ -> notimpl()
                | Tuple xs ->
                    xs |> List.iteri  (fun i x ->
                        il.Emit(OpCodes.Dup)
                        emitExpression il (Literal(Int(i)))
                        let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                        il.EmitCall(OpCodes.Call, mi, null)
                        deconstruct x
                    )
            deconstruct pattern
            il.Emit(OpCodes.Pop)
        | SetAt(loc,e) -> emitSetAt il (loc,e)
        | Action(invoke) -> emitInvoke il invoke
        | PropertySet(tn,n,e) -> emitPropertySet il tn n e
        | If(condition) -> 
            let elseLabel = il.DefineLabel()
            let endLabel = il.DefineLabel()
            ifStack.Push(elseLabel,endLabel)
            emitExpression il condition
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, elseLabel)
        | ElseIf(condition) ->
            let elseLabel,endLabel = ifStack.Pop()
            il.Emit(OpCodes.Br,endLabel) //Unconditionally transfers control to a target instruction
            il.MarkLabel(elseLabel)
            let elseLabel = il.DefineLabel()
            ifStack.Push(elseLabel,endLabel)
            emitExpression il condition
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse,elseLabel) //Transfers control to a target instruction if a value is false
        | Else -> 
            let elseLabel,endLabel = ifStack.Pop()
            il.Emit(OpCodes.Br,endLabel) //Unconditionally transfers control to a target instruction
            il.MarkLabel(elseLabel)
            ifStack.Push(endLabel,endLabel) //Is this correct?
        | EndIf -> 
            let elseLabel,endLabel = ifStack.Pop()
            il.MarkLabel(elseLabel)
            if elseLabel <> endLabel then il.MarkLabel(endLabel)
        | For((Set(name,x)) as set, until, step) -> 
            let beginFor = il.DefineLabel()
            let endFor = il.DefineLabel()
            loopStack.Push(beginFor,endFor)
            //Initialize counter
            emitSet il set
            //Skip step on first iteration
            let compare = il.DefineLabel()
            il.Emit(OpCodes.Br, compare)//Unconditionally transfers control to a target instruction
            // Begin for loop
            il.MarkLabel(beginFor)
            // Step
            emitExpression il (Arithmetic(Var(name),Add,step))
            il.Emit(OpCodes.Stsfld,fieldLookup name)
            // Compare
            il.MarkLabel(compare)
            // Is step positive or negative (+ve or -ve)
            let positiveStep = il.DefineLabel()
            let cont = il.DefineLabel()
            emitExpression il (Comparison(step,Le,Literal(Int(0))))
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse,positiveStep)
            // negative step -ve
            emitExpression il (Comparison(Var(name),Ge,until))
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse,endFor)
            il.Emit(OpCodes.Br,cont)
            // positive step +ve
            il.MarkLabel(positiveStep)
            emitExpression il (Comparison(Var(name),Le,until))
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse,endFor)
            il.MarkLabel(cont)
        | While(condition) -> 
            let beginWhile = il.DefineLabel()
            let endWhile = il.DefineLabel()
            loopStack.Push(beginWhile,endWhile)
            il.MarkLabel(beginWhile)
            emitExpression il condition
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse,endWhile)
        | EndFor | EndWhile -> 
            let beginLoop,endLoop = loopStack.Pop()
            il.Emit(OpCodes.Br,beginLoop)
            il.MarkLabel(endLoop)
        | Goto(name) ->
            let label = obtainLabel il name
            il.Emit(OpCodes.Br,label)
        | Label(name) -> 
            let label = obtainLabel il name
            il.MarkLabel(label)
        | Sub(name,_) | Function(name,_) -> 
            let builder,_ = methods.[name]
            methodName := name
            methodIL := builder.GetILGenerator()
        | EndSub -> 
            il.Emit(OpCodes.Ret)
            methodName := "Main"
            methodIL := mainIL
        | EndFunction -> 
            il.Emit(OpCodes.Ldsfld,fieldLookup !methodName)
            il.Emit(OpCodes.Ret)
            methodName := "Main"
            methodIL := mainIL
        | Select(e) -> 
            emitExpression il e
            let endLabel = il.DefineLabel()
            caseStack.Push(None)
        | Case(clauses) ->
            let endLabel = 
                match caseStack.Pop() with
                | Some(caseLabel,endLabel) ->
                    il.Emit(OpCodes.Br,endLabel)
                    il.MarkLabel(caseLabel)
                    endLabel
                | None -> il.DefineLabel()
            let caseLabel = il.DefineLabel()
            caseStack.Push(Some(caseLabel,endLabel))
            let emitCompare op value =
                il.Emit(OpCodes.Dup) //Copies the top most value on the stack, then pushes the copy onto the stack
                emitLiteral il value
                let mi = typeof<Primitive>.GetMethod(toOp op)
                il.EmitCall(OpCodes.Call,mi,null)
                emitConvertToBool il
            let rec emitClause (matchLabel:Label) = function
                | Any -> il.Emit(OpCodes.Br, matchLabel)
                | Is(op,value) -> 
                    emitCompare op value
                    il.Emit(OpCodes.Brtrue, matchLabel)
                | Range(from,until) ->
                    let below = il.DefineLabel()
                    emitCompare Lt from
                    il.Emit(OpCodes.Brtrue,below)
                    emitCompare Le until
                    il.Emit(OpCodes.Brtrue,matchLabel)
                    il.MarkLabel(below)
                | Pattern(Tuple(patterns)) ->
                    let failLabel = il.DefineLabel()
                    //Check is array
                    il.Emit(OpCodes.Dup)
                    let mi = typeof<Microsoft.SmallBasic.Library.Array>.GetMethod("IsArray")
                    il.EmitCall(OpCodes.Call,mi,null)
                    emitConvertToBool il
                    il.Emit(OpCodes.Brfalse,failLabel)
                    //Check item count
                    il.Emit(OpCodes.Dup)
                    let mi = typeof<Microsoft.SmallBasic.Library.Array>.GetMethod("GetItemCount")
                    il.EmitCall(OpCodes.Call,mi,null)
                    emitLiteral il (Int(patterns.Length))
                    let mi = typeof<Primitive>.GetMethod(toOp Eq)
                    il.EmitCall(OpCodes.Call,mi,null)
                    emitConvertToBool il
                    il.Emit(OpCodes.Brfalse,failLabel)
                    // Check items
                    patterns |> List.iteri (fun i pattern ->
                        match pattern with
                        | Bind("_") -> ()
                        | Clause clause -> 
                            let itemLabel = il.DefineLabel()
                            il.Emit(OpCodes.Dup)
                            emitExpression il (Literal(Int(i)))
                            let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                            il.EmitCall(OpCodes.Call,mi,null)
                            emitClause itemLabel clause
                            il.Emit(OpCodes.Pop)
                            il.Emit(OpCodes.Br,failLabel)
                            il.MarkLabel(itemLabel)
                            il.Emit(OpCodes.Pop)
                        | _ -> notimpl()
                    )
                    il.Emit(OpCodes.Br,matchLabel)
                    il.MarkLabel(failLabel)
                | Pattern _ -> notimpl()
            let clauseLabel = il.DefineLabel()
            //clauses |> List.iter (emitClause clauseLabel)
            for clause in clauses do emitClause clauseLabel clause
            il.Emit(OpCodes.Br,caseLabel)
            il.MarkLabel(clauseLabel)
        | EndSelect -> 
            match caseStack.Pop() with
            | Some(caseLabel,endLabel) ->
                il.MarkLabel(caseLabel)
                il.MarkLabel(endLabel)
            | None -> ()
            il.Emit(OpCodes.Pop)
    for (pos,instruction) in program do
        let il = !methodIL
        il.MarkSequencePoint(doc,pos.StartLn,pos.StartCol,pos.EndLn,pos.EndCol)
        emitInstruction il instruction
/// Compiles program instructions to a .Net assembly
let compileTo name program =
    let _, instructions = program |> Array.unzip
    /// Builder for assembly
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            AssemblyName(name),
            AssemblyBuilderAccess.RunAndSave)
    /// Builder for module
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(name+".exe",true)
    /// Writer for source links
    let doc = moduleBuilder.DefineDocument(name+".sb", Guid.Empty,Guid.Empty,Guid.Empty)
    /// Builder for type
    let typeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.Public)
    /// Fields representing program's variables
    let fields = generateFields typeBuilder instructions
    /// Methods representing program's subroutine
    let methods = generateMethods typeBuilder instructions
    /// Main method representing main routine
    let mainBuilder =
        typeBuilder.DefineMethod( 
            "_Main",
            MethodAttributes.Static ||| MethodAttributes.Public,
            typeof<Void>,
            [|typeof<string[]>|])
    let args = mainBuilder.DefineParameter(1, ParameterAttributes.None, "args")
    let il = mainBuilder.GetILGenerator()
    // Emit program instructions
    emitInstructions il doc methods fields program
    il.Emit(OpCodes.Ret)
    // Set main method as entry point
    assemblyBuilder.SetEntryPoint(mainBuilder)
    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(name+".exe")

