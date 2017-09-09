open System
//random list generator
let genRandomNumbers count =
    let rnd = System.Random()
    List.init count (fun _ -> rnd.Next (1, 100))


// 2.1 Lav en funktion mindste(A,n) der returnerer det mindste tal i en tabel A med n tal
let list1 = [8;3;9;7;2;4]
let mindste (a:int list) n =
    let mutable min = a.[0]
    for i = 1 to n-1 do
        if a.[i] < min then
            min <- a.[i]
    min
// 2.2 [*] Lav en funktion der bestemmer om to tal fra tabellen summer til 100

let ranList1 = genRandomNumbers 100

let findSumOfTwo (a:int list) n x =
    let sortedList = List.sort a
    let mutable lo = 0
    let mutable hi = n-1
    let mutable found = false

    while lo < hi && found = false do
        let sum = sortedList.[lo] + sortedList.[hi]
        printfn "high = %d\t low = %d\t sum =%d" sortedList.[hi] sortedList.[lo] sum
        if sum = x then
            found <- true
        elif sum < x then
            lo <- (lo + 1)
        else
            hi <- (hi - 1)
    found