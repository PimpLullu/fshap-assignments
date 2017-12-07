open System
open System.Text.RegularExpressions

let cr = Console.ReadLine
let pfs = printfn "%s"
let pfn = printfn 
let pfd = printfn "%d"

let wrong = "Invalid input, try again!"

let files = "Which file do you wish to print?"
let sfile = "File succesfully printet! Do you wish to print another file?"
let naFile = "No such file, try again!"

let web = "Please type a webpage"

let wcsc = "Welcome to simpleCalc"
let mutable ans = 0

let rtrn = "Returning to the main menu, "
let pick = "type one of the following options:\n"
let bye = "Goodbye!"
let mm = "Type m or menu, to return to main menu."
let quit = "To quit, type q or exit."

let items = pick+
            "1 for printFile\n"+
            "2 for printWebpage\n"+
            "3 for simpleCalc\n"+
            quit

"Hello, "+items |> pfs

let rec run (input:string) =

// 9ø.0
// Lav en funktion, (printFile : unit -> unit) som starter en
// dialog med brugeren. Programmet skal spørge brugeren om
// navnet på en fil, og derefter skrive filens indhold ud på skærmen.

    let printFile (file:string) =
        
        let print (reader:IO.StreamReader) =
            while not (reader.EndOfStream) do
                reader.ReadLine() |> pfs

        let rec find = function
            | s when s="q"||s="exit"  -> pfs bye
            | s when s="m"||s="menu"  -> rtrn+items |> pfs ; cr() |> run
            | s when IO.File.Exists s -> let reader = IO.File.OpenText s
                                         print reader ; pfs sfile
                                         pfs mm ; pfs quit ; cr() |> find
            | _                       -> naFile |> pfs ; cr() |> find

        file |> find
 
// 9ø.1
// Lav en funktion, (printWebPage : url:string -> string) som
// indlæser indholdet af internetsiden på url og returnerer
// resultatet som en streng.

    let rec printWebPage (url:string) =

        try let url2Stream url =
                let uri = Uri url
                let request = Net.WebRequest.Create uri
                let response = request.GetResponse()
                response.GetResponseStream()

            let readUrl url =
                let reader = new IO.StreamReader(url |> url2Stream)
                reader.ReadToEnd()
            readUrl url |> pfs
            
         with :? Net.WebException as ex -> pfs "Webpage does not exists!"

        pfs "Try again with another webpage."
        pfs mm ; pfs quit

        let webMenu = function
            | s when s="exit"||s="q" -> bye |> pfs
            | s when s="m"||s="menu" -> rtrn+items |> pfs ; cr() |> run
            | s                      -> "http://"+s |> printWebPage

        cr() |> webMenu 

// 9ø.2
// Lav en lommeregner, (simpleCalc : unit -> unit) som starter en
// uendelig dialog med en bruger. Brugeren skal kunne indtaste simple
// regnestykker på postive heltal, og hvert regnestykke må kun bestå af
// en enkelt af følgende binære operatorer: +, -, *, /. Resultatet skal
// kunne genbruges i den efterfølgende beregning med navnet ans.

    let rec simpleCalc (input:string) =

        let num = input.Split ('+','-','/','*')

        let plus (num:string array) =
            match num with
            | num when num.[0] = "ans" -> ans <- ans+(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc 
            | num when num.[1] = "ans" -> ans <- (int num.[0])+ans
                                          ans |> pfd ; cr() |> simpleCalc
            | _                        -> ans<-(int num.[0])+(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc

        let minus (num:string array) =
            match num with
            | num when num.[0] = "ans" -> ans <- ans-(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc 
            | num when num.[1] = "ans" -> ans <- (int num.[0])-ans
                                          ans |> pfd ; cr() |> simpleCalc
            | _                        -> ans<-(int num.[0])-(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc

        let divide (num:string array) =
            match num with
            | num when num.[0] = "ans" -> ans <- ans/(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc 
            | num when num.[1] = "ans" -> ans <- (int num.[0])/ans
                                          ans |> pfd ; cr() |> simpleCalc
            | _                        -> ans<-(int num.[0])/(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc

        let times (num:string array) =
            match num with
            | num when num.[0] = "ans" -> ans <- ans*(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc 
            | num when num.[1] = "ans" -> ans <- (int num.[0])*ans
                                          ans |> pfd ; cr() |> simpleCalc
            | _                        -> ans<-(int num.[0])*(int num.[1])
                                          ans |> pfd ; cr() |> simpleCalc

       (*let mutable sLength = input.Length
        
        let rec oString = function
            | (s,i) when i < sLength   -> s
            | (s,i) when input.[i]='+' -> oString (s+"+",i+1)
            | (s,i) when input.[i]='-' -> oString (s+"-",i+1)
            | (s,i) when input.[i]='/' -> oString (s+"/",i+1)
            | (s,i) when input.[i]='*' -> oString (s+"*",i+1)
            | (s,i)                    -> oString (s,i+1)

        let oS = oString ("", 0)
                
        let funk = function
            | (_,l) when l=0 -> ans |> pfd ; cr() |> simpleCalc
            | (i,_) when oS.[i]='+' -> plus
            | (i,_) when oS.[i]='-' -> minus
            | (i,_) when oS.[i]='/' -> divide
            | (i,_) when oS.[i]='*' -> times *)

        let calcMenu (s:string) =
            match s with
            | s when s="exit"||s="q" -> bye |> pfs
            | s when s="m"||s="menu" -> rtrn+items |> pfs ; cr() |> run
            | s when s.Contains("+") -> num |> plus 
            | s when s.Contains("-") -> num |> minus
            | s when s.Contains("/") -> num |> divide       
            | s when s.Contains("*") -> num |> times
            | _                      -> wrong |> pfs ; cr()|> simpleCalc
            

        (*let checkInput (a:string array) =
            match a with
            | [||]                              -> input |> calcMenu
            | a when a.[0]="ans" || a.[1]="ans" -> input |> calcMenu
            | a when (Regex.Matches(a.[0],@"[a-zA-Z]").Count) > 0
                                                -> wrong |> pfs
                                                   cr() |> simpleCalc
            | _                                 -> input |> calcMenu

        num |> checkInput*)
        
        input |> calcMenu


    let rec menu = function
        | s when s = "exit" || s = "q" -> pfs bye
        | s when s = "1" -> pfs files ; cr() |> printFile 
        | s when s = "2" -> pfs web ; "http://"+cr() |> printWebPage
        | s when s = "3" -> pfs wcsc ; cr() |> simpleCalc
        | _              -> pfs wrong ; cr() |> menu

    input |> menu

cr() |> run
