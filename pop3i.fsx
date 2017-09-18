open System
//3ø.0
let a = 3
let b = 4
let x = 5
printfn "%A * %A + %A = %A" a x b (a * x + b)

let y = a * x + b
printfn "%A * %A + %A = %A" a x b y

// Vi bruger paranteserne til at lave udregningen inden i
// vi behøver ikke parenteserne når vi har gemt udregningen i variable
/////////////////////////////////////////////////////////
//3ø.1
let a = 3 in let b = 4 in let x = 5 in let y = (a*x+b) in do printfn "%A * %A + %A = %A" a x b y

////////////////////////////////////////////////////////
//3ø.2
let firstName = "Jon"
let lastName = "Sporring"
let name = firstName + " " + lastName
printfn "Hello %A!" name
let firstName = "Jon" in let lastName = "Sporring" in let name = firstName+" "+lastName in do printfn "Hello %A!" name;;

//////////////////////////////////////////////////////
//3ø.3
let f a b x =
    let y = a*x+b
    y
printfn "%A * %A + %A = %A" a x b (f a b x)

/////////////////////////////////////////////////////
//3ø.4
let f2 a b =
    for x=0 to 5 do
        let y = a*x+b
        printfn "%A * %A + %A = %A" a x b y

let f3 a b =
    let mutable x = 0
    while x <= 5 do
        let y = a*x+b
        printfn "%A * %A + %A = %A" a x b y
        x <- x + 1

/////////////////////////////////////////////////////
//3ø.5
let pt x =
    for i=1 to x do
        if (i%10) = 0 then
            printfn "%4d" i
        else 
            printf "%4d" i
//Whoops i did it wrong. I really need to learn how to read the assignment
//Correct anwser
let multiplicationTable =
    printf "      "
    for i=1 to 10 do
        printf "%4i" i
    printf "\n"
    printfn "      ________________________________________"
    for i=1 to 10 do
        printf "%4i |" i
        for j=1 to 10 do
            if j=10 then 
                printfn "%4i" (i*j)
            else
                printf "%4i" (i*j)

//////////////////////////////////////////////////
//3ø.6

