open System
open System.Collections.Generic

//random list generator
let dummy = 8
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

(*  Zombieduellering Du har en hær af n hjernetomme zombier. Du vil gerne finde den stærkeste og svageste
    zombie blandt gruppen. Ved at sætte to zombier sammen i et bur med en luns kød kan du hurtigt afgøre
    hvilken af de to er stærkest. Desværre slider det på zombier at slås, så du vil gerne minimere antallet af dueller.
    Løs følgende opgaver.    *)

// 4.1 Vis hvordan du finder den stærkeste zombie med højst n − 1 dueller.

let zomlist = [5;8;4;9;2;6]
let findStrongestZombie (a:int array) n =
    let mutable strong = a.[0]
    let mutable index = 0
    for i = 1 to n-1 do
        if a.[i] > strong then
            strong <- a.[i]
            index <- i
    [index+1;strong]

let findWeakestZombie (a:int array) n =
    let mutable weak = a.[0]
    let mutable index = 0
    for i = 1 to n-1 do
        if a.[i] < weak then
            weak <- a.[i]
            index <- i
    [index+1;weak]

//4.2 [*] Vis hvordan du finder både den stærkeste og svageste zombie med højst 3n/2 dueller.
let zomlist2 = [|5;8;15;9;2;6;12;4;8;3;11;16;5;7;6;13;10|]

let findWeakAndStrong (a:int array) n =
    let strongA = [|1..n/2|]
    let weakA   = [|1..n/2|]
    let strong  = a.[n-1]
    let weak    = a.[n-1]

    for i in 0..2..n-2 do
        if a.[i] > a.[i+1] then  
            strongA.[i/2] <- a.[i]; weakA.[i/2] <- a.[i+1]
        else
            strongA.[i/2]<-a.[i+1]; weakA.[i/2] <- a.[i+1]


    printfn "Strongest zombie:"
    findStrongestZombie strongA (n/2)
    printfn "Weakest zombie:"
    findWeakestZombie weakA (n/2)