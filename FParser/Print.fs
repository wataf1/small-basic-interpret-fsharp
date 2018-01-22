module Print
open System
type IPrintable =
    abstract member Formatted :unit->string


let inline sjoin (xs:seq<#IPrintable>) =
    let sarr = xs |> Seq.map (fun x -> x.Formatted())
    let s = String.Join(";", sarr)
    sprintf "[%s]" s

let pfmt x = (x:>IPrintable).Formatted()
let inline pprint1 s a = sprintf s ((a:>IPrintable).Formatted())
let inline pprint2 s a b = sprintf s ((a:>IPrintable).Formatted()) ((b:>IPrintable).Formatted())
let inline pprint3 s a b c = sprintf s ((a:>IPrintable).Formatted()) ((b:>IPrintable).Formatted()) ((c:>IPrintable).Formatted())
let inline pprint4 s a b c d = sprintf s ((a:>IPrintable).Formatted()) ((b:>IPrintable).Formatted()) ((c:>IPrintable).Formatted()) ((d:>IPrintable).Formatted())
