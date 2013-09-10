

    
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

let dado = dist { let! u = Dist.uniform in return int (u * 6.0) + 1 }

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

let risikoItaliano = dist {
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