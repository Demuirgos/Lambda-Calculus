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
    
    let expect c = 
        let innerProcess chars = 
            match chars with
            | head :: tail when c = head -> Success(head,tail)
            | _ -> Failure(sprintf "Expected %c" c)
        Parser innerProcess
    
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
    
    let apply f param = 
        (f .>>. param) |> map (fun (f,x) ->f x) 
    let (<*>) = apply
    
    let give result = 
        let innerProcess str = 
            Success(result,str)
        Parser innerProcess
    
    let rec lift2 f param1 param2=
        give f <*> param1 <*> param2
    
    let rec sequence parsers = 
        let cons a b = a :: b
        let (++)  = lift2 (cons)
        match parsers with
        | [] -> give []
        | parser::rest -> parser ++ (sequence rest)
         
    let allOf = 
        List.map (expect) 
        >> sequence

