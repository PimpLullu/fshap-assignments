open System
//1.1 Declare a function g: int -> int, where g (n) = n + 4.abs
let addFour n =
    n + 4

//1.2 Declare a function h: float * float -> float, where h (x, y) = root(x^2 + y^2). Hint: Use the function System.Math.Sqrt.
let pow2 x = x*x
let pythagoras x y =
    Math.Sqrt((pow2 x)+(pow2 y))

//1.3 Write function expressions corresponding to the functions g and h in the exercises 1.1 and 1.2.
let addFour1 = fun n -> n+4
let addFour2 = function
    | n -> n+4

let pythagoras1 = fun x y -> Math.Sqrt((pow2 x)+(pow2 y)) //pythagoras 3.0 4.0
let pythagoras2 = function // pythagoras (3.0, 4.0)
    | x, y -> Math.Sqrt((pow2 x)+(pow2 y)) 

//1.4 Declare a recursive function f: int -> int, where f (n) = 1 + 2 + · · · + (n − 1) + n
// for n ≥ 0 . (Hint: use two clauses with 0 and n as patterns.) 
//State the recursion formula corresponding to the declaration. Give an evaluation for f (4) .
let rec partialSum = function
    | 1 -> 1
    | n -> n + partialSum n-1
 
//1.5 The sequence F 0 , F 1 , F 2 , . . . of Fibonacci numbers is defined by:
//F = 0     F = 1       F(n) = F n−1 + F n−2
//Thus, the first members of the sequence are 0, 1, 1, 2, 3, 5, 8, 13, . . . .
//Declare an F# function to compute F n . Use a declaration with three clauses, where the patterns
//correspond to the three cases of the above definition. Give an evaluations for F4.
let rec fib = function
    | 0 | 1 -> 1
    | n -> fib(n-1) + fib(n-2)

//1.6 Declare a recursive function sum: int * int -> int, where
//sum (m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
//for m ≥ 0 and n ≥ 0 . (Hint: use two clauses with (m,0) and (m,n) as patterns.)
//Give the recursion formula corresponding to the declaration.

let rec sum = function
    | m, 0 -> 0
    | m, n -> m + sum(m, (n-1))


//1.7 Determine a type for each of the expressions:
//(System.Math.PI, fact -1) // fact -1 crashes the interactive console & float, int
//fact(fact 4) = int = -775946240
//power(System.Math.PI, fact 2) = float = 9.869604401
//(power, fact) = val it : (float * int -> float) * (int -> int)
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)
let x = System.Math.PI, fact -1
let y = fact(fact 4)
let rec power = function
    | (x,0) -> 1.0
    | (x,n) -> x * power(x,n-1)
let z = power(System.Math.PI, fact 2)

//1.8 Consider the declarations:    let a = 5;;     let f a = a + 1;;   let g b = (f b) + a;;
//Find the environment obtained from these declarations and write the evaluations of the expres-
//sions f 3 and g 3.
//envioment is a,f and g. f(3) = 4. g(3) = 9
let a = 5;
let f a = a + 1
let g b = (f b) + a