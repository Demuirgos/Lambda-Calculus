module Abstractor 
    open Interpreter
    open Parsec
    type Thunk = Interpreter.Expression 
    type Statement = 
        | Identifier        of string 
        | Bind              of Statement * Statement * Statement 
        | Function            of Statement list * Statement 
        | Application       of Statement * Statement list
    and BinaryOp   = Add | Subs | Div | Mult      
    #nowarn "40"

    let parseExpr = 
        let rec parseLet =
            Parser {
                let consumeLet = "let" |> Seq.toList |> allOf
                let consumeIn  = "in" |> Seq.toList |> allOf
                let consumeEq  = expect '='
                let binds = parseExpression 
                return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. binds
                                  .>>  pSpaces .>> consumeIn  .>> pSpaces .>>. binds
            } <?> "Binder" |>> (fun ((a,b),c) -> (a,b,c) |> Bind)
        and parseIdentifier = 
            Parser {
                return! ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            } <?> "Identifier" |>> (toString >> Identifier)
        and parseFunction  = 
            Parser {
                let pArrow = "=>" |> Seq.toList |> allOf
                let pArgs = many 1 parseIdentifier 
                let mParams =  ','  |> expect >>. pSpaces
                                    |> separate1By pArgs
                                    |> betweenC ('(',')') 
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Function" |>> (fun (args, value) -> (List.concat args, value) |> Function)
        and parseOperation  = 
            Parser {
                let pArgs=  ','  |> expect >>. pSpaces
                                    |> separate1By parseExpression
                                    |> betweenC ('(',')')
                return! parseIdentifier .>>. pArgs
            } <?> "Applicative" |>> Application
        and parseExpression = 
            Parser {
                let! expr = 
                    choice [    
                        parseLet
                        parseOperation      
                        parseFunction
                        parseIdentifier    
                    ] 
                return expr
            } <?> "Expression" 
        parseLet
    let transpile input = 
        // make this function emit lambda AST instead of parsable strings
        let rec curry =
            function 
            | Function ([h],_) as input ->
                input
            | Function (h::t,body) ->
                Function ([h], curry <| Function(t, body))
            | _ -> failwith "Expression cannot be curried"

        let Result = (fromStr input, parseExpr) 
                     ||> run
        match Result with
        | Success (program,_) -> 
            let rec emitLambda= function
                | Bind(name, expr, value) ->
                    sprintf "(\\%s.%s %s)" (emitLambda name) (emitLambda value) (emitLambda  expr)
                | Function(parameters, expr) as f->
                    let emit = function | Function([param], expr) -> sprintf "\\%s.%s" (emitLambda param) (emitLambda expr)
                                        | _ -> failwithf "%A is not a lambda, transpiling failed" (nameof f)
                    match parameters with 
                    | [] | [_] -> emit f
                    | _ -> emit (curry f)
                | Application(expr, args) ->
                    let operation = sprintf "%s" (emitLambda expr)
                    let rec wrap op = function
                        | [] -> op
                        | h::t -> wrap (sprintf "(%s %s)" op (emitLambda h)) t
                    sprintf "%s" (wrap operation args)
                | Identifier(name) -> name
            emitLambda program
        | Failure _ -> toResult Result
    
    let rec uncompile =
        function 
        | Applicative(f, args) ->
            Application(uncompile f, [uncompile args])
        | Lambda(p, b) -> 
            Function([uncompile p], uncompile b)
        | Atom(name) -> Identifier(name)