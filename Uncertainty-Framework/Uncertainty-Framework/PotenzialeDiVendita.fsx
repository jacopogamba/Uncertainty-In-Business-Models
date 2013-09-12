#I @"C:\Users\JACOPO\Documents\GitHub\Uncertainty-In-Business-Models\lixely\lixely-fsharp\bin\Release"
#r @"RandomTools.dll"
#r @"distr.dll"

#load @"C:\Users\JACOPO\Documents\GitHub\Uncertainty-In-Business-Models\Uncertainty-Framework\packages\FSharp.Charting.0.84/FSharp.Charting.fsx"

open FSharp.Charting
open Distributions
open RandomVariables

let bucket (x:float) (y:seq<float>) =
    let f v = floor (v * x) / x
    y |> Seq.countBy f |> Seq.toArray |> Array.sortBy fst


type Sesso =
    | Maschio
    | Femmina

let sesso,eta =
    let totpersoneusa = 304280
    let getsessoeta n =
        let demograficacumulativa =
            let demograficausa =
                [|
                    10955,10479
                    10624,10162
                    10178,9714
                    10719,10367
                    10684,10470
                    10928,10525
                    9761,9871
                    9902,9986
                    10172,10387
                    11092,11435
                    10692,11169
                    9318,9854
                    7667,8556
                    5672,6349
                    4063,4873
                    3020,4161
                    2408,3375
                    1631,3062
                |] 
            let foldalo (fascia,(_, smf)) (m, f) =
                fascia + 5 ,(smf + m, smf + m + f)
            Seq.scan foldalo (0,(0,0)) demograficausa |> Seq.skip 1 |> Seq.toList

        let fascia, (maxmaschi, maxfemmine) = Seq.find (fun (fascia,(m,f)) -> if f>n then true else false) demograficacumulativa
        let sesso = if n<maxmaschi then Maschio else Femmina
        sesso,fascia

    let sessoeta = rndvar {
        return dist {
            let! u = Dist.uniform 
            let sesso,fascia = getsessoeta (int (u*(float totpersoneusa)))
            let! u = Dist.uniform
            let eta = (int (u*5.0)) + fascia - 5
            return sesso,eta
        }
    }
    rndvar {
         let! sesso,eta = sessoeta
         return dist { return sesso }
    }, rndvar {
         let! sesso,eta = sessoeta
         return dist { return eta }
    }


Dist.getSampleSeq (getDist sesso) (gen()) |> Seq.take 3042800 |> Seq.countBy (fun x -> match x with Maschio -> "Maschio"| Femmina -> "Femmina" ) |> Chart.Column
Dist.getSampleSeq (getDist eta) (gen()) |> Seq.take 3042800 |> Seq.countBy (fun x ->x) |> Chart.Column

let contaPerEta s = Seq.countBy (fun (sesso,eta) -> eta) s

Dist.getSampleSeq (getDist (rndvar{let! s = sesso in let! e =eta in return dist{return s,e} }) ) (gen()) |>
    Seq.take 1000000 |>
    Seq.groupBy fst |>
    Seq.map (fun (sesso,sequenza) -> contaPerEta sequenza|> Seq.sort |>Chart.Column)|>Chart.Combine //match sesso with Maschio -> "maschio"| Femmina -> "femmina") |>
    
