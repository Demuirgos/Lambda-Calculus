module Interpreter
    open Parsec
    open FSharp.Core
    #nowarn "40"
    type Expression = 
        | Atom of string
        | Applicative of Expression * Expression
        | Function of string list * Expression 
        | Lambda of string * Expression 
    and 'a Output = 
        | Value of 'a
        | Failed of string
    and Envirement = list<string * Expression>

    let rec curry =
        function 
        | Function (arguments,body) ->
            match arguments with 
            | [arg]       -> Lambda (arg,body)
            | arg :: args -> Lambda (arg, (args, body) |> Function |> curry )
            | _ -> body
        | _ -> failwith "Expression cannot be curried"
    
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
                let pLmbda = anyOf [ '\\'; 'λ']; 
                let pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                let pVars = pVar .>> option (pSpaces) |> many 1  
                let pDot = expect '.'
                let! func = pLmbda >>. pVars .>> pDot .>>. parseExpression 
                return func
            } <?> "Function" |>> fun (var,body) -> Function (var |> List.map toString ,body)
        and parseLambda = 
            Parser {
                let pLmbda = anyOf [ '\\'; 'λ']; 
                let pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                let pDot = expect '.'
                let! lmbda = pLmbda >>. pVar .>> pDot .>>. parseExpression 
                return lmbda
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
    
    let evalExpr input= 
        match input with
        | Parsec.Parser.Success (term,_) -> 
            let rec occurs identifier = 
                function
                | Atom id -> 
                    let isThere = id = identifier
                    (isThere, isThere)
                | Lambda (arg, body) -> 
                    let inArg = arg = identifier
                    let inBody = (identifier, body) ||> occurs |> fst 
                    (inArg || inBody, inArg && inBody)
                | Function (_) as f ->
                    f |> curry |> occurs identifier
                | Applicative(lhs, rhs) -> 
                    let isThere = (identifier,lhs) ||> occurs |> fst || (identifier,rhs) ||> occurs |> fst 
                    (isThere, isThere)
            let rec ``α Convert`` id lambda=  
                let rec convert old rep expr = 
                    let convert' = convert old rep
                    match expr with 
                    | Atom(id) -> 
                        if id = old then Atom rep
                        else expr
                    | Applicative   (lhs, rhs)   -> Applicative (convert' lhs, convert' rhs)
                    | Function      (args, body) -> Function    (args,convert' body)
                    | Lambda        (arg, body)  -> Lambda      (arg,convert' body)
                match occurs id lambda with 
                | (true,_) -> 
                   Failed (sprintf "New name '%s' already appears in %A" id lambda)
                | _ -> match lambda with 
                       | Lambda (arg,body) -> Lambda (id, convert arg id body) |> Value 
                       | Function(_) as f -> 
                            f 
                            |> curry 
                            |> ``α Convert`` id
                       | _ -> Failed (sprintf "α-conversion not supported for %A" lambda)
            let (/>) = ``α Convert``

            let rec ``β-redex`` =
                let allVariables expr =
                    let rec loop expr : seq<string> =
                        seq {
                            match expr with
                                | Atom name -> yield name
                                | Applicative (func, arg) ->
                                    yield! loop func
                                    yield! loop arg
                                | Lambda (param, body) ->
                                    yield param
                                    yield! loop body
                                | Function (_) as f ->
                                    yield! f |> curry |> loop 
                        }
                    loop expr |> Set.ofSeq
                let rec substitute arg param body =
                    let substitute' = substitute arg param  
                    match body with
                    | Atom id ->
                        if id = param then Value arg
                        else Value body 
                    | Applicative(fn, arg) -> 
                        match substitute' fn, substitute' arg  with 
                        | Value(fn'), Value(arg') ->  Value (Applicative(fn',arg'))
                        | (Failed(msg) ,_) | (_, Failed (msg)) -> Failed msg 
                    | Lambda(local, body') -> 
                        if local = param then Value body 
                        else 
                            let occurence = (local, arg)
                                            ||> occurs
                                             |> snd
                            if occurence then 
                                let localVars = allVariables body
                                let result = ['a'..'z']
                                             |> Seq.map (fun c -> c.ToString())
                                             |> Seq.tryFind (not << localVars.Contains)
                                             |> Option.map (fun repl -> 
                                                 match repl /> body with
                                                 | Value v -> substitute' <| v 
                                                 | Failed _ as error ->  error )
                                match result with 
                                | Some v -> v
                                | _ -> Failed "Exhausted variable names for α-conversion"
                            else
                                match substitute' body' with 
                                | Value body'' -> Lambda (local, body'') |> Value
                                | Failed _ as error -> error 
                    | Function(_) as f -> 
                        f |> curry |> substitute'
                function
                | Applicative (Lambda (param, body), arg) ->
                    substitute arg param body
                | expression -> Failed <| sprintf "%A is not a β-redex" expression
            let (</) () expr= ``β-redex`` expr

            let evaluate expression = 
                let rec isBetaRedex = 
                    function
                    | Atom _ -> false
                    | Applicative (Lambda(_), _) -> true
                    | Applicative (Function(_), _) -> true
                    | Applicative (lhs, rhs) -> 
                        isBetaRedex lhs || isBetaRedex rhs
                    | Lambda (_, body) ->
                        isBetaRedex body
                    | Function(_) as f -> 
                        f |> curry |> isBetaRedex
                let rec reduce expr = 
                    match expr with 
                    | Atom _ -> Value expr
                    | Applicative (Lambda(_), _) -> 
                        () </ expr 
                    | Lambda (arg, body) ->
                        match reduce body with 
                        | Value body' -> 
                            Lambda (arg, body') |> Value 
                        | error -> error
                    | Applicative(lhs, rhs) ->
                        let lhsc,rhsc = isBetaRedex lhs,isBetaRedex rhs 
                        match lhsc,rhsc with 
                        | (true, _) -> 
                            match reduce lhs with 
                            | Value v ->  Applicative (v, rhs) |> Value
                            | error -> error
                        | (_, true) -> 
                            match reduce rhs with 
                            | Value v ->  Applicative (lhs, v) |> Value
                            | error -> error
                        | _ -> Value expr
                    | Function(_) as f -> 
                        f |> curry |> reduce 
                let rec loop expr =
                    match isBetaRedex expr with 
                    | true ->   expr 
                                |>  reduce 
                                |>  function 
                                    | Value expr' -> loop expr'
                                    | error -> error
                    | _ -> Value expr
                loop expression                          
            evaluate term
        | error -> error |> toResult |> Failed  

    let toString expr = 
        match expr with 
        | Value v ->
            let rec toString' term = 
                match term with 
                | Lambda(arg,body) -> sprintf "λ%s.%s" arg (toString' body)
                | Atom(v) -> sprintf "%s" v
                | Applicative(lhs,rhs) -> sprintf "(%s %s)" (toString' lhs) (toString' rhs)
                | Function(_) as f -> 
                    f |> curry |> toString' 
            toString' v
        | Failed e -> e
        
    let interpret input = 
        (fromStr input, parseExpr .>> eof) 
            ||> run 
             |> evalExpr
             |> toString
