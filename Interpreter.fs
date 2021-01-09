module Interpreter
    #nowarn "40"
    type Expression = 
        | Atom of string
        | Applicative of Expression * Expression
        | Function of string list * Expression 
        | Lambda of string * Expression 
        | Promise of string * Expression * Envirement
    and Envirement = list<string * Expression>
    type 'a Output = 
        | Value of 'a
        | Failed of string
    let parseExpr = 
        let rec parseAtom =
            Parser {
                let! atom = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                return atom
            } <?> "Atom" |>> (toString >> Atom)
        and parseApp = 
            Parser {
                let pParens p = between (expect '(') p (expect ')')
                let! app = pParens ( parseExpression .>> pSpaces .>>. parseExpression )
                return app 
            } <?> "Applicative" |>> Applicative
        and parseFunction = 
            Parser {
                let pLmbda = expect 'λ'
                let pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                let pVars = pVar .>> option (pSpaces) |> many 1  
                let pDot = expect '.'
                let! func = pLmbda >>. pVars .>> pDot .>>. parseExpression 
                return func
            } <?> "Function" |>> fun (var,body) -> Function (var |> List.map toString ,body)
        and parseLambda = 
            Parser {
                let pLmbda = expect 'λ'
                let pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                let pDot = expect '.'
                let! func = pLmbda >>. pVar .>> pDot .>>. parseExpression 
                return func
            } <?> "Lambda" |>> fun (var,body) -> Lambda (var |> toString ,body)
        and parseExpression  = 
            Parser {
                let! expr = 
                    choice [    
                        parseAtom   ;  
                        parseApp    ;    
                        parseLambda ;
                        parseFunction    
                    ] 
                return expr
            } <?> "Expression" 
        parseExpression
    let evalExpr expr = 
        let rec curry =
            function 
            | Function (arguments,body) ->
                match arguments with 
                | [arg]       -> Lambda (arg,body)
                | arg :: args -> Lambda (arg, (args, body) |> Function |> curry  )
                | _ -> body
            | _ -> failwith "Expression cannot be curried"
        // let rec ``α Convert`` old rep expr  =  
            // match expr with 
            // | Atom(_) -> 
                // if expr = old then rep
                // else expr
            // | Applicative   (lhs, rhs)   -> Applicative (``α Convert`` old rep lhs, ``α Convert`` old rep rhs)
            // | Function      (args, body) -> Function    (args,``α Convert`` old rep body)
            // | Lambda        (arg, body)  -> Lambda      (arg,``α Convert`` old rep body)
        let rec eval env expr =
            match expr with
            | Atom name -> 
                match List.tryFind (fun (id,_) -> name = id) env with
                | Some (_, term) -> Value term
                | _ -> Failed (sprintf "Couldn't find a term with the id : %s" name)
            | Applicative (fn, param) -> 
                match eval env fn with 
                | Value (Promise (arg, body, image)) -> 
                    match eval env param with
                    | Value v -> 
                        eval ((arg,v)::image @ env) body
                    | Failed(_) -> Failed ("Couldn't complete the evaluation operation 2")
                | _ -> Failed ("Couldn't complete the evaluation operation 1")
            | Function(_) as f -> 
                let lamdaish = curry f
                eval env lamdaish 
            | Lambda(arg,body) -> 
                Promise (arg,body,env) |> Value
            | Promise(_) as p -> Value p
        match expr with 
        | Some (v) -> 
            Some (eval [] v)
        | _ -> None
    let toString expr = 
        match expr with 
        | Some r -> 
            match r with 
            | Value v ->
                let rec toString' term = 
                    match term with 
                    | Lambda(arg,body) | Promise (arg,body,_) -> sprintf "λ%s.%s" arg (toString' body)
                    | Atom(v) -> sprintf "%s" v
                    | Applicative(lhs,rhs) -> sprintf "(%s %s)" (toString' lhs) (toString' rhs)
                    | _ -> ""
                toString' v
            | Failed e -> e
        | None -> "Error : No value was produced"
        