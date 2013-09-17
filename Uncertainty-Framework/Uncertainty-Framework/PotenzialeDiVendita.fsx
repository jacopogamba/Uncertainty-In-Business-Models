#I @"C:\Users\pq\Documents\GitHub\Uncertainty-In-Business-Models\lixely\lixely-fsharp\bin\Release"
#r @"RandomTools.dll"
#r @"distr.dll"

#load @"C:\Users\pq\Documents\GitHub\Uncertainty-In-Business-Models\Uncertainty-Framework\packages\FSharp.Charting.0.84/FSharp.Charting.fsx"

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
    
    (*dati inventati
let etaprobcellulareM = [ 5,0.0; 10,0.02; 15,0.28; 20,0.8; 25,0.9; 30,0.9; 35, 0.9; 40, 0.9; 45,0.85; 50,0.8;55,0.75;60,0.6;65,0.3;70,0.2;75,0.1;80,0.0;85,0.0;10000,0.0]
let etaprobcellulareF = [ 5,0.0; 10,0.02; 15,0.30; 20,0.8; 25,0.9; 30,0.9; 35, 0.9; 40, 0.9; 45,0.85; 50,0.8;55,0.75;60,0.6;65,0.3;70,0.2;75,0.1;80,0.0;85,0.0;10000,0.0]

let smartphone = rndvar {
    let! sesso = sesso
    let tabella = match sesso with Maschio -> etaprobcellulareM | Femmina -> etaprobcellulareF
    let! eta = eta
    let _,probabilita = Seq.find (fun (fascia,_) ->  fascia>eta ) tabella
    return dist {
        let! u = Dist.uniform
        return u < probabilita
    }
}*)

// calcolato 
let p_smartphone = 
    let p_smartphone_se_adulto = 0.56 
    let p_adulto_se_smartphone = 1.0 - 0.063
    let p_adulto = 1.0 - (0.07 + 0.068 + 0.065 + 0.069 * 3.0 / 4.0)
    p_smartphone_se_adulto * p_adulto / p_adulto_se_smartphone 
    

let smartphone = rndvar { 
    let! e = eta
    let p_bambino = 0.07 + 0.068 + 0.065 + 0.069 * 3.0 / 4.0
    return dist {
        let! u = Dist.uniform
        let prob = match e with
                    | e when e < 18 -> 0.063 * p_smartphone / p_bambino
                    | e when e < 25 -> 0.79
                    | e when e < 35 -> 0.81
                    | e when e < 45 -> 0.69
                    | e when e < 55 -> 0.55
                    | e when e < 65 -> 0.39
                    | _             -> 0.18
        return u < prob
    }
}

let rec smartphoneeta etaMin etaMax = 
    let smartphoneEta = getDist (rndvar {
        let! s = smartphone
        let! e = eta
        return dist { return s,e }
        })
    dist {
        let! s,e = smartphoneEta
        if e >= etaMin && e < etaMax then
            return s
        else
            let! s = smartphoneeta etaMin etaMax
            return s       
    }

let rec etaSmartphone () = 
    let smartphoneEta = getDist (rndvar {
        let! s = smartphone
        let! e = eta
        return dist { return s,e }
        })
    dist {
        let! s,e = smartphoneEta
        if s then
            return e
        else
            let! e = etaSmartphone ()
            return e   
    }

Dist.getSampleSeq ( smartphoneeta 60 100) (gen()) |> Seq.take 304280 |> Seq.countBy (fun (x:bool) -> if x then "Hanno lo smartphone" else "Non ce l'hanno" ) |> Chart.Pie
Dist.getSampleSeq ( etaSmartphone ()) (gen()) |> Seq.take 304280 |> Seq.countBy (fun x -> x ) |> Chart.Column


let stressLevel = rndvar  {
    let! eta = eta
    //let! sesso = sesso
    let slMean,slVariance =
        match eta with
            | e when e < 25 -> 16.78, 6.86
            | e when e < 35 -> 17.46, 7.31
            | e when e < 45 -> 16.38, 7.07
            | e when e < 55 -> 16.94, 7.83
            | e when e < 65 -> 14.50, 7.20
            | _             -> 11.09, 6.77
    return Dist.normal slMean  slVariance
}

let stressed = rndvar {
    //we consider a person stressed if his/her stress level is above  0.546023 standard deviations in order to have about 22% stressed people in total
    let minStressLevel = 0.772193 * 7.50 + 15.83
    let! stressLevel = stressLevel
    return dist {
        return stressLevel > minStressLevel
    }
}

let compraApplicazione = rndvar {
    let! smartphone = smartphone
    if not smartphone then return dist { return false }
    else
        let! stressed = stressed
        return dist {
            let! u = Dist.uniform
            return u < if stressed then 0.1 else 0.01
        }
}


Dist.getSampleSeq ( getDist stressed) (gen()) |> Seq.take 1000000 |> Seq.countBy (fun (x:bool) -> if x then "Stressati" else "Tranquilli" ) |> Chart.Pie


let compraPerEta conteggio =
    let compraEta = rndvar {
        let! compra = compraApplicazione
        let! eta = eta
        return dist { return compra,eta }
        }
    let totpersoneusa = 304280 * 1000
    let utenti = dist {
        let! compra,eta = getDist compraEta
        return if compra then Some eta else None
        }
    Dist.getSampleSeq utenti (gen()) |> Seq.take conteggio |> Seq.filter (function None -> false | _ -> true) |> Seq.countBy (fun  x -> 5 * int (0.2 * float x.Value) ) |> Seq.map (fun (eta,i) -> eta,(float i) * (float totpersoneusa )/ (float conteggio) ) 


Chart.Column (compraPerEta 1000000)