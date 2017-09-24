module RGB

type Color = int*int*int
let c = (10,60,100):Color

let rec trunc (r,g,b):Color =
    match (r,g,b) with
    | (r,_,_) when (r,g,b) < (0,g,b)    -> trunc (0,g,b)
    | (r,_,_) when (r,g,b) > (255,g,b)  -> trunc (255,g,b)
    | (_,g,_) when (r,g,b) < (r,0,b)    -> trunc (r,0,b)
    | (_,g,_) when (r,g,b) > (r,255,b)  -> trunc (r,255,b)
    | (_,_,b) when (r,g,b) < (r,g,0)    -> trunc (r,g,0)
    | (_,_,b) when (r,g,b) > (r,g,255)  -> trunc (r,g,255)
    | (_,_,_)                           -> (r,g,b)

let add (r1,g1,b1) (r2,g2,b2) =
    trunc(r1+r2,g1+g2,b1+b2)

let scale a (r,g,b) =
    trunc(a*r,a*g,a*b)

let gray (r,g,b) =
    let (r2,g2,b2) = trunc(r,g,b)
    (r2+g2+b2)/3

let grayTriplet (r,g,b):Color =
    let c = gray(r,g,b)
    (c,c,c)