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






    
let input = seq[1;3;6;4]

input |> Seq.sort |> Seq.zip input |> Seq.map (fun (x,y) -> x + y) |> Seq.fold (fun stato valore -> stato + valore) 0

let isEven n =
    n % 2 = 0

input |> Seq.filter isEven

input |> Seq.map (fun x -> float x) |> Seq.average 

input |> Seq.fold (fun (somma,conteggio) valore -> (somma + valore,conteggio + 1) ) (0,0) |> fun (somma,conteggio) -> (float somma)/(float conteggio)

Seq.initInfinite (fun i -> i) |> Seq.map (fun x -> x % 5) |> Seq.take 20 |> Seq.toList

isEven 17


let attacco = [| 1 ; 2; 6 |]
let difesa = [| 2 ; 2 ; 5 |]
//quante volte perde attacco?
Seq.zip attacco difesa|> Seq.map (fun (a,d) -> if a > d then 0 else 1) |> Seq.fold (+)  0

let dadoleo = dist { let! u = Dist.uniform in return int (u * 6.0) + 1 }

let rec dadi n = dist {
    if n = 0 then
        return []
    else
        let! altri = dadi (n-1)
        let! d = dado
        return d::altri
}

let risikoItaliano = dist {
    let! a1 = dado
    let! a2 = dado
    let! a3 = dado
    let! d1 = dado
    let! d2 = dado
    let! d3 = dado
    let attacco = [| a1;a2;a3|]
    let difesa = [| d1;d2;d3|]
    Array.sortInPlace attacco
    Array.sortInPlace difesa
    return 
        Seq.zip attacco difesa|> Seq.map (fun (a,d) -> if a > d then 0 else 1) |> Seq.fold (+)  0
}

let risikoItalianoleo = dist {
    let! attacco = dadi 3
    let! difesa = dadi 3
    let attacco = List.toArray attacco
    let difesa = List.toArray difesa
    Array.sortInPlace attacco
    Array.sortInPlace difesa
    return 
        Seq.zip attacco difesa|> Seq.map (fun (a,d) -> if a > d then 0 else 1) |> Seq.fold (+)  0
}

let risikoGenerale dadiAttacco dadiDifesa = dist {
    let! attacco = dadi dadiAttacco
    let! difesa = dadi dadiDifesa
    let attacco = List.toArray attacco
    let difesa = List.toArray difesa
    let neg a = -a
    Array.sortInPlaceBy neg attacco
    Array.sortInPlaceBy neg difesa
    return 
        Seq.zip attacco difesa|> Seq.map (fun (a,d) -> if a > d then (0,1) else (1,0)) |> Seq.fold (fun (a,b) (c,d) -> a + c , b + d)  (0,0)
}


let rec chivince carrarmatiAttacco carrarmatiDifesa = dist {
    let dadiAttacco = min carrarmatiAttacco 3
    let dadiDifesa = min carrarmatiDifesa 3
    let! (persiAttacco,persiDifesa) = risikoGenerale dadiAttacco dadiDifesa
    if persiAttacco = carrarmatiAttacco then // l'attacco ha finito i carrarmati
        return "Difesa"
    else if persiDifesa = carrarmatiDifesa then //la difesa ha finito i carrarmati
        return "Attacco"
    else
        let! continua = chivince (carrarmatiAttacco-persiAttacco) (carrarmatiDifesa-persiDifesa)
        return continua
}

Dist.getSampleSeq (chivince 10 3) (gen()) |> Seq.take 1000 |> Seq.countBy (fun x -> x) |> Seq.sortBy fst |> Chart.Column

Dist.getSampleSeq (dadi 3) (gen()) |> Seq.take 5 |> Seq.toList


type Facce =
    | Testa
    | Croce

let moneta = dist {
    let! u = Dist.uniform
    if u > 0.5 then return Testa else return Croce
    }

let Monete =
    let Moneta1 = rndvar { return moneta }
    let Moneta2 = rndvar { return moneta }
    let Moneta3 = rndvar { return moneta }
    rndvar {
        let! m1 = Moneta1
        let! m2 = Moneta2
        let! m3 = Moneta3
        return dist { return m1,m2,m3 }
    }

let Moneta1 = rndvar {
    let! foglietto = Monete
    let m1, m2,m3 = foglietto
    return dist { return m1 }
    }

let Moneta2 = rndvar {
    let! foglietto = Monete
    let m1, m2,m3 = foglietto
    return dist { return m2 }
    }

let MonetaUguale = rndvar {
    let! m1 = Moneta1
    let! m2 = Moneta2
    return dist { return  m1 = m2 }
    }

Dist.getSampleSeq ( getDist MonetaUguale ) (gen()) |> Seq.take 1000 |> Seq.countBy (fun x -> x) |>  Chart.Pie


let f = fun x -> match x with None -> false | _ -> true
let f1 = function None -> false | Some x -> true

type Option<'T> =
    | Some of 'T
    | None