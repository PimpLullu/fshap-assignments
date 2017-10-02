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
// Splits a list into 2 lists, half through the list.
let split (xs:int list) =
    let mutable x = (xs.Length/2)
    if (xs.Length%2) = 1 then
        x <- x + 1
    let xs1 = xs.[..x]
    let xs2 = xs.[x+1..]
    (xs1,xs2)

// Splits a list into 2 lists, by even and odd numbers.
let split2 (xs:int list) =
    let mutable xs1 = []
    let mutable xs2 = []
    for i=xs.Length-1 downto 0 do
        if (xs.[i] % 2) = 0 then
            xs1 <- xs.[i]::xs1
        else
            xs2 <- xs.[i]::xs2
    (xs1,xs2)
        
// Splits a list into 2 lists, one by one.
let split3 (xs:int list) =
    let mutable xs1 = []
    let mutable xs2 = []
    for i=xs.Length-1 downto 0 do
        if (i % 2) = 0 then
            xs1 <- xs.[i]::xs1
        else
            xs2 <- xs.[i]::xs2
    (xs1,xs2)
//5ø.3
let reverseApply x f =
    f x
    
//5ø.4
// int -> (int -> int) is a int function, that uses another
// int -> int function.
let f1 x y =
    x+y
let f2 x =
    f1 x

// (int -> int) -> int  
let f3 f:int =
    f 5 

//5ø.5
let evens (xs:int list) =
     xs |> List.filter (fun x -> x % 2 = 0) 
  
//5ø.6
    
//5ø.7
    
//5ø.8
    
//5ø.9
let squares n =
    let a = [|1..n|]
    for i in a do
        a.[i-1] <- i*i
    a
    
//5ø.10
    
//5ø.11

//5ø.12
