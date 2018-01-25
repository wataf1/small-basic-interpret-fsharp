module Sample
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
//type Type1(data:string) =
//    static member StaticProperty :string
//    new() = Type1("default")
//    member this.InstanceProperty = data.Length
//    member this.InstanceMethod(i:int)= data.[i]
    //type NestedType() = 
        //static member StaticProperty1 : string
        //static member StaticProperty100 : string
[<TypeProvider>]
type SampleTypeProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)

    let namespaceName = "Samples.HelloWorldTypeProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()
    let makeOneProvidedType (n:int) = 
        let t = ProvidedTypeDefinition(thisAssembly, namespaceName, "Type" + string n,baseType = Some typeof<obj>)
        t.AddXmlDocDelayed(fun () -> sprintf "This provided type %s" ("Type" + string n))
        let sp = ProvidedProperty("StaticProperty", typeof<string>, isStatic=true,getterCode= (fun _ -> <@@ "Hello!" @@>))
        sp.AddXmlDocDelayed(fun _ -> "This is static!")
        t.AddMember(sp)
        let ctor = ProvidedConstructor([], fun args -> <@@ "The object data" :> obj @@>)
        ctor.AddXmlDocDelayed(fun _ -> "this is ctor")
        t.AddMember ctor
        let ctor2 = ProvidedConstructor([], fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        ctor2.AddXmlDocDelayed(fun _ -> "this is ctor2")
        t.AddMember ctor2
        let ip = ProvidedProperty("InstanceProperty", typeof<int>,getterCode= (fun args -> <@@ ((%%(args.[0]):obj) :?> string).Length @@>))
        ip.AddXmlDocDelayed(fun _ -> "This is instance prop!")
        t.AddMember(ip)

        let meth = 
            ProvidedMethod("InstanceMethod",[ProvidedParameter("x",typeof<int>)], typeof<char>, 
                fun args -> <@@ ((%%(args.[0]):obj):?>string).Chars(%%(args.[1]):int)@@>)
        meth.AddXmlDocDelayed(fun _ -> "This is meth!")
        t.AddMember meth

        t.AddMembersDelayed(fun _ ->
            let nestedType = ProvidedTypeDefinition("NestedType", Some typeof<obj>)
            nestedType.AddMembersDelayed( fun () -> 
                [for i in 1..100 do 
                    let v = sprintf "string #%i" i
                    let p = 
                        ProvidedProperty(sprintf "StaticProperty%i" i, typeof<string>,isStatic=true, 
                            getterCode=(fun _ -> <@@ v @@>))
                    p.AddXmlDocDelayed(fun () -> sprintf "This is StaticProperty%d on NestedType" i)
                    yield p])
            [nestedType])
        t
    let types = [for i in 1..100 -> makeOneProvidedType i]
    do this.AddNamespace(namespaceName, types)


do()