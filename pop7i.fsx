//7ø.0
let rec insert y = function
    | []              -> [y]
    | h::t when h > y -> y::h::t
    | h::t            -> h::insert y t

let isort xs = List.fold (fun acc x -> insert x acc) [] xs

let xs = [7;55;34;23;5;42;32;34;8]

isort xs |> printfn "%A"

//7ø.1
let rec bubble = function
    | []                   -> []
    | h::[]                -> [h]
    | h::t when h < t.Head -> h::bubble t
    | h::t                 -> t.Head::bubble (h::t.Tail) 

let bsort xs = List.fold (fun acc _ -> bubble acc) xs xs

bsort xs |> printfn "%A"
//7ø.2

let tList0 = []
let tList1 = [4]
let tList2 = [6;3]
let tList3 = [10..-1..0]
let tList4 = [60;24;1;1;3;24;5;3;40]

isort tList0 |> printfn "%A"
isort tList1 |> printfn "%A"
isort tList2 |> printfn "%A"
isort tList3 |> printfn "%A"
isort tList4 |> printfn "%A"

bsort tList0 |> printfn "%A"
bsort tList1 |> printfn "%A"
bsort tList2 |> printfn "%A"
bsort tList3 |> printfn "%A"
bsort tList4 |> printfn "%A"

//7ø.3

open ImgUtil

let rec triangle bmp len (x , y ) =
    if len < 25 then
        setBox blue (x , y ) ( x + len , y + len ) bmp
    else
        let half = len / 2
        do triangle bmp half ( x + half /2 , y )
        do triangle bmp half (x , y + half )
        do triangle bmp half ( x + half , y + half )
        
do runSimpleApp " Sierpinski " 600 600 ( fun bmp -> triangle bmp 512 (30 ,30) | > ignore )

//7ø.4
