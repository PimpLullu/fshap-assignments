open System
//2.1

let abe = [0..5]
let sum (a:int list) n =
    let mutable x = a.[0]
    for i=1 to n-1 do
        x <- x + a.[i]
    x

let missing (a:int list) n =
    let mutable value = sum a n





