// 5ø.0
let oneToN n =
    [1..n]

//5ø.1
let multiplicity x (xs:int list) =
    let mutable count = 0
    for i=0 to (xs.Length-1) do
        if x = xs.[i] then
            count <- count + 1
    count

//5ø.2
let split (xs:int list) =
    let mutable x = (xs.Length/2)
    if (xs.Length%2) = 1 then
        x <- x + 1
    let xs1 = xs.[..x]
    let xs2 = xs.[x+1..]
    (xs1,xs2)

//5ø.3
    
//5ø.4
    
//5ø.5
let evens (xs:int list) =
    (fun x -> x % 2) |>  List.filter in xs 
  
//5ø.6
    
//5ø.7
    
//5ø.8
    
//5ø.9
let squares n =
    let a = [|0..n-1|]
    for i=1 to n-1 do
        a.[i] <- i*i
    a
    
//5ø.10
    
//5ø.11

//5ø.12
