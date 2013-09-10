#I @"C:\Users\JACOPO\Documents\GitHub\Uncertainty-In-Business-Models\lixely\lixely-fsharp\bin\Release"
#r @"RandomTools.dll"
#r @"distr.dll"

#load @"C:\Users\JACOPO\Documents\GitHub\Uncertainty-In-Business-Models\Uncertainty-Framework\packages\FSharp.Charting.0.84/FSharp.Charting.fsx"

open FSharp.Charting
open Distributions


let marketsizedistribution = dist {
    return 2.0 //milioni di $
}

Dist.getSampleSeq marketsizedistribution (gen())

Dist.uniform

Dist.getSampleSeq Dist.uniform (gen())

Chart.Line (seq{for x in 0..10 -> x, x * x})


let bucket (x:float) (y:seq<float>) =
    let f v = floor (v * x) / x
    y |> Seq.countBy f |> Seq.toArray |> Array.sortBy fst

Dist.getSampleSeq Dist.uniform (gen()) |> Seq.take 1000000 |> bucket 100.0|> Chart.Point 


let moneta = 
    dist {
        let! v = Dist.uniform 
        return
         if v < 0.5 then "testa"
         else "croce" 
    }

Dist.getSampleSeq moneta (gen()) |> Seq.take 10000 |> Seq.countBy (fun x -> x) |> Chart.Pie

let dado =
    dist {
        let! v = Dist.uniform
        return
            if v < 1.0/6.0 then 1
            else if v < 2.0/6.0 then 2
            else if v < 3.0/6.0 then 3
            else if v < 4.0/6.0 then 4
            else if v < 5.0/6.0 then 5
            else 6
    }

Dist.getSampleSeq dado (gen()) |> Seq.take 10000 |> Seq.countBy (fun x -> x) |> Seq.sortBy fst |> Chart.Column

let sommaduedadi = dist {
    let! a = dado
    let! b = dado
    let! c = dado
    return a + b + c 
}

Dist.getSampleSeq sommaduedadi (gen()) |> Seq.take 10000 |> Seq.countBy (fun x -> x) |> Seq.sortBy fst |> Chart.Column

let risultatirisiko = dist {
    let! attacco1 = dado
    let! attacco2 = dado
    let! attacco3 = dado
    let! difesa1 = dado
    let! difesa2 = dado
    let attacco = [|attacco1; attacco2; attacco3|]
    Array.sortInPlace attacco
    let difesa = [|difesa1; difesa2|]
    Array.sortInPlace difesa
    let attaccoperdeil1 = attacco.[2] <= difesa.[1] 
    let attaccoperdeil2 = attacco.[1] <= difesa.[0]
    return
        if attaccoperdeil1 then 
            if attaccoperdeil2 then 2
            else 1
        else if attaccoperdeil2 then 1
        else 0
    }

Dist.getSampleSeq risultatirisiko (gen()) |> Seq.take 100000 |> Seq.countBy (fun x -> x) |> Seq.sortBy fst |> Chart.Column

let risikoitaliano = dist {
    let! attacco1 = dado
    let! attacco2 = dado
    let! attacco3 = dado
    let! difesa1 = dado
    let! difesa2 = dado
    let! difesa3 = dado
    let attacco = [|attacco1; attacco2; attacco3|]
    let difesa = [|difesa1; difesa2; difesa3|]
    Array.sortInPlace attacco
    Array.sortInPlace difesa
    return 
        Seq.zip attacco difesa |> Seq.map (fun (att, dif) -> if att > dif then 0 else 1) |> Seq.fold (+) 0 

}

Dist.getSampleSeq risikoitaliano (gen()) |> Seq.take 100000 |> Seq.countBy (fun x -> x) |> Seq.sortBy fst |> Chart.Pie