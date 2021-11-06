[<AutoOpen>]
module Parsec
    open FSharp.Core
    
    let inline toString l = l |> ((List.map string) >> List.reduce (+))
    
    type Error = string
    [<AutoOpen>]
    module Position =
        type Position = Cursor of (int*int)
        let ``initial Position`` =  Cursor (0,0)
        let incrCol = 
            function
            | Cursor(line,column) -> Cursor(line,column + 1)
        let incrLin = 
            function
            | Cursor(line,_) -> Cursor(line + 1,0)
    [<AutoOpen>]
    module State =
        type State = Input of (char list[] * Position)
        let fromStr str = 
            if String.length str = 0 then
                Input ([||],``initial Position``)
            else
                let separators = [| '\r'; '\n' |]
                let lines = separators |> str.Split  |> Array.map Seq.toList
                Input (lines,``initial Position``)
        let currentLine = 
            function
            | Input (lines,Cursor (line,_)) ->
                if line < Array.length lines then
                    Some lines.[line]
                else
                    None   
        let next input = 
            match input with
            | Input (lines,pos) -> 
                let current = currentLine input
                match (current,pos) with
                | (None,_) -> input, None
                | (Some l,Cursor(_,column)) -> 
                    match column < List.length l with 
                    | true  -> (Input(lines,incrCol pos),Some l.[column])
                    | false -> (Input(lines,incrLin pos),Some '\n')
    [<AutoOpen>]
    module Parser =
        [<AutoOpen>]
        module ParserPosition = 
            type ParserPosition = {
                Marker : Position
                Line : char list Option
            }
            let fromState state= 
                match state with 
                | Input (_,position) ->
                    {Marker = position; Line = currentLine state} 

        type Result<'a> =
            | Success of 'a 
            | Failure of label:string * message:Error * location:ParserPosition
        type Parser<'a> = {
            Function: (State -> Result<'a * State>)
            Label : string
        }
    
        let toResult result =
            match result with
            | Success (value,_) -> 
                sprintf "%A" value
            | Failure (label,error,cursor) -> 
                let line, colPos,linePos = 
                    match cursor.Line,cursor.Marker with
                    | (Some l,Cursor (lin,col)) -> toString l,col,lin
                    | (None  ,Cursor (lin,col)) -> "EOF"     ,col,lin
                let caret = sprintf "%*s^ %s" colPos "" error
                sprintf "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label line caret 
        
        let getContent = 
            function
            | Success (v,_) -> Some v
            | _ -> None

        let ref p = lazy (p)  
    
        let run word p = word |> p.Function 
    
        let bind f p =
            let innerProcess input = 
                match run input p with
                | Failure (label, msg, pos) -> Failure (label, msg, pos)
                | Success(parsed, left) -> 
                    run left (f parsed)   
            {Function = innerProcess; Label="unknown"}
        let (>>=) f p = bind p f
        
        let give result = 
            let innerProcess str = 
                Success(result,str)
            {Function=innerProcess; Label= sprintf "%A" result}
        
        let empty state = Failure ("Empty", "pzero", fromState state) 
        
        type ParserMonad() =
            member inline _.Return(x)  = give x
            member inline _.ReturnFrom(P)  = P
            member inline _.Bind(p, f) = p >>= f
            member inline _.Delay(f) = 
                let promise = Lazy.Create f
                () |> give |> bind (fun ()->promise.Value)
            member inline __.Zero() = empty
        let Parser = ParserMonad()
    
        let apply fP xP = 
            Parser {
                let! f = fP
                let! x = xP
                return f x
            }
        let (<*>) = apply
    
        let setLabel parser newLabel = 
            let innerProcess input = 
                match parser.Function input with
                | Success s ->
                    Success s 
                | Failure (_,err,pos) -> 
                    Failure (newLabel,err,pos)  
            {Function=innerProcess; Label=newLabel}
        let (<?>) = setLabel
            
        let (<%>) = toResult
            
        let satisfy pred label= 
            let innerProcess input =
                let tail,head = next input 
                match head with
                | Some char when pred char -> Success (char, tail)
                | Some char -> Failure (label,  sprintf "Unexpected '%c'" char, fromState input)
                | _  -> Failure (label,  sprintf "Unexpected character", fromState input) 
            {Function=innerProcess; Label=sprintf "satisfy %A" pred}
    [<AutoOpen>]
    module Primitives =             
        let expect c = satisfy (fun prefix -> prefix = c) (sprintf "%c" c)

        let orElse parser1 parser2  = 
            let innerProcess str= 
                match run str parser1 with
                | Success(parsed,left) -> Success(parsed,left)
                | _ -> run str parser2 
            {Function=innerProcess; Label=sprintf "%s orElsa %s" (parser1.Label) (parser2.Label)}
        let (<|>) = orElse
            
        let anyOf  = 
            List.map (expect) 
            >> List.reduce (orElse)
        
        let andThen parser1 parser2 =
            Parser {
                let! a = parser1
                let! b = parser2
                return (a,b)
            } <?> sprintf "%s andThen %s" (parser1.Label) (parser2.Label)
        let (.>>.) = andThen
        
        let map f parser =
            Parser {
                let! c = parser
                return f c
            }
        let (|>>) x f = map f x
        
        let rec lift2 f param1 param2=
            give f <*> param1 <*> param2
        
        let add = lift2 (+)

        let startWith =
            let innerProcess (str:string) (prefix:string) = str.StartsWith(prefix)
            lift2 innerProcess
        
        let endWith =
            let innerProcess (str:string) (suffix:string) = str.EndsWith(suffix)
            lift2 innerProcess

        let rec sequence parsers = 
            let cons a b = a :: b
            let (++)  = lift2 (cons)
            match parsers with
            | [] -> give []
            | parser::rest -> parser ++ (sequence rest)

        let allOf = 
            List.map (expect) 
            >> sequence 

        let tryWith parser word = 
            match run word parser with
            | Success (parsed,left)   -> Success (parsed,left)
            | Failure (lbl, msg, pos) -> Failure (lbl, msg, pos)
        let (/>?) word parser = tryWith parser word

        let keepParsing offset parser =
            let innerProcess input = 
                let initialParser =
                    let seq = 
                        Seq.initInfinite (fun _ -> parser ) 
                        |> Seq.take offset
                        |> Seq.toList
                    seq |> sequence
                let rec loop input parser =
                    match run input parser with
                    | Success (firstValue,inputAfterFirstParse) ->
                        let (subsequentValues,remainingInput) = loop inputAfterFirstParse parser
                        let values = firstValue::subsequentValues
                        (values,remainingInput)
                    | _ -> ([],input)
                match run input initialParser with 
                | Failure (label, msg, pos) when offset <> 0 -> Failure (label, msg, pos)
                | _ -> Success (loop input parser)
            {Function = innerProcess; Label = sprintf "%s{%d,}" parser.Label offset}
        let many n parser = keepParsing n parser
                
        let option parser = 
            let some = parser |>> Some
            let none = give None
            (some <|> none) <?> (sprintf "opt %s" (parser.Label))
        
        let choice parsers = 
            List.reduce (<|>) parsers

        let (.>>) lhs rhs = 
            lhs .>>. rhs
            |> map (fun (a,_) -> a)

        let (>>.) lhs rhs = 
            lhs .>>. rhs
            |> map (fun (_,b) -> b)

        let between left parser right = 
            left >>. parser .>> right

        let separate1By parser separator =
            parser .>>. many 0 (separator >>. parser )
            |>> (fun (h,l) -> h::l) 
        
        let eof = 
            let innerProcess input= 
                match next input with
                | (_,Some c) when c <> '\n' -> 
                    Failure ("EOF","Expected EOF Token", fromState input)
                | _ -> 
                    Success(fromStr "",input)
            {Function = innerProcess; Label = "EOF"}
    [<AutoOpen>]
    module Predefined = 
        let pSpaces = many 0 (expect ' ')
        let cleanStr s = s |> String.filter (function | ' ' | '\n' -> false | _ -> true)