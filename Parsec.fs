[<AutoOpen>]
module Parsec
    open FSharp.Core
    
    type Result<'a,'b> =
        | Success of 'a 
        | Failure of 'b
    type Parser<'a> = Parser of ((char list) -> Result<'a * char list,string>)
    
    let run word = 
        function
        | Parser func -> func word
    
    let empty = Success((),"")

    let bind f p =
        let innerProcess input = 
            match run input p with
            | Failure msg -> Failure (msg)
            | Success(parsed,left) -> 
                run left (f parsed)   
        Parser innerProcess
    let (>>=) = bind

    let give result = 
        let innerProcess str = 
            Success(result,str)
        Parser innerProcess
    
    let satisfy pred = 
        let innerProcess input = 
            match input with
            | head :: tail when pred head -> Success(head, tail)
            | _  -> Failure(sprintf "Unexpected character") 
        Parser innerProcess

    let expect c = satisfy (fun prefix -> prefix = c)
        
    let orElse parser1 parser2  = 
        let innerProcess str= 
            match run str parser1 with
            | Success(parsed,left) -> Success(parsed,left)
            | _ -> run str parser2 
        Parser innerProcess
    let (<|>) = orElse
    
    let anyOf = 
        List.map (expect) 
        >> List.reduce (orElse)
    
    let andThen parser1 parser2 =
        let innerProcess str= 
            match run str parser1 with
            | Success(parsed1,left1) -> match run left1 parser2 with 
                                        | Success(parsed2,left2) -> Success((parsed1,parsed2),left2)
                                        | Failure(msg) -> Failure(msg)
            | Failure(msg) -> Failure(msg)
        Parser innerProcess
    let (.>>.) = andThen
    
    let map f parser =
        let innerProcess str = 
            match run str parser with
            | Success (parsed,left) -> Success(f parsed,left)
            | Failure msg -> Failure(msg)
        Parser innerProcess
    let (<!>) = map

    let apply f param = 
        (f .>>. param) |> map (fun (f,x) ->f x) 
    let (<*>) = apply
    
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
       failwith "Not yet made"

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
                | Failure err ->
                    ([],input)
                | Success (firstValue,inputAfterFirstParse) ->
                    let (subsequentValues,remainingInput) = loop inputAfterFirstParse parser
                    let values = firstValue::subsequentValues
                    (values,remainingInput)
            match run input initialParser with 
            | Failure msg when offset <> 0 -> Failure msg
            | _ -> Success (loop input parser)
        Parser innerProcess
    
    let many n parser = 
        keepParsing n parser

    let option parser = 
        let some = parser |>> Some
        let none = give None
        some <|> none

    let (.>>) lhs rhs = 
        lhs .>>. rhs
        |> map (fun (a,_) -> a)
        
    let (>>.) lhs rhs = 
        lhs .>>. rhs
        |> map (fun (_,b) -> b)

    let between left parser right = 
        left >>. parser .>> right
    
    let separateBy parser separator =
        parser .>>. many 0 (parser .>> separator)
        |>> (fun (head,tail) -> head::tail) 

    type ParserMonad() =
        member inline __.Delay(f)   = fun state -> (f ()) state
            member inline __.Return(x)  = give x
            member inline __.Bind(p, f) = p >>= f
            member inline __.Zero()     = empty
            member inline __.ReturnFrom(p) = p
            member inline __.TryWith(p, cf) =
              fun state -> try p state with e -> (cf e) state
            member inline __.TryFinally(p, ff) =
              fun state -> try p state finally ff ()
    let Parser = ParserMonad()