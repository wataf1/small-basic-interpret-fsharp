open System
open System.Collections
open System.Collections.Generic

let arr = [|0;1;2;3;2;1;2;4;5;6;7|]
let arr2 = [|0..10|]

let freq (xs:int array) =
    xs |> Array.maxBy (fun x -> xs |> Array.filter ((=) x))

let pairs (xs:int array) =
    seq{

        for i = 0 to xs.Length - 1 do
            for j = 0 to xs.Length - 1 do
                let x,y = xs.[i],xs.[j]
                if (x + y) = 10 then yield (x,y)
    }       


let fib n =
    let lookup = Array.create (n+1) 0

    let rec fib' n =
        match n with 
        | 1 | 2 -> 1
        | x -> 
            match lookup.[x] with
            | 0 ->
                lookup.[x] <- fib'(x-1) + fib'(x-2)
                lookup.[x]
            | x -> x
    fib' n
let count (xs: 'a array) x = (xs |> Array.filter (fun y -> x = y)).Length

let findOneElement (xs: 'a array) =
    xs |> Array.find (fun x -> (count xs x) = 1)

let common (a:int array) (b:int array) =
    let ha = new HashSet<int>(a)
    //let hb = new HashSet<int>(b)
    ha.IntersectWith(b)
    ha |> Array.ofSeq


let rec bin (xs:int array) (x:int) =
    let mid = xs.Length / 2
    let mv = xs.[mid]
    if x = mv then
        mid
    elif x > mv then 
        bin xs.[mid..] x
    else
        bin xs.[0..mid] x


let isPrime n =
    let sqrt' = (float >> sqrt >> int) n
    [2..sqrt'] |> List.forall (fun x -> n % x <> 0)

let allPrimes =
    let rec allPrimes' n =
        seq {
            if isPrime n then yield n
            yield! allPrimes' (n+1)
        }
    allPrimes' 2

let primes max =
    let array = new BitArray(max,true)
    let lastp = Math.Sqrt(float max) |> int
    for p in 2..lastp+1 do
        if array.Get(p) then
            for pm in p*2..p..max-1 do
                array.Set(pm,false)
    seq { for i in 2..max-1 do if array.Get(i) then yield i }

