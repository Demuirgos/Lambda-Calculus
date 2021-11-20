module Abstractor 
    open Interpreter
    open Parsec
    type Thunk = Interpreter.Expression 
    type Statement = 
        | Value             of int
        | Identifier        of string 
        | YComb             of Statement
        | Bind              of Statement * Statement * Statement 
        | Function          of Statement list * Statement 
        | Application       of Statement * Statement list
        | Mathematic        of Statement * Operation * Statement
    and Operation   =   Add | Subs | Div | Mult | Exp
                        static member toOp token =
                            match token with 
                            | "*" -> Mult | "/" -> Div
                            | "-" -> Subs | "^" -> Exp 
                            | "+" -> Add  | _ -> failwith "Failed Parsing"
    #nowarn "40"

    let parseExpr = 
        let rec parseLet =
            Parser {
                let consumeLet = "let"|> Seq.toList |> allOf
                let consumeIn  = "in" |> Seq.toList |> allOf
                let consumeEq  = ":=" |> Seq.toList |> allOf
                let binds = parseExpression 
                return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. binds
                                  .>>  pSpaces .>> consumeIn  .>> pSpaces .>>. binds
            } <?> "Binder" |>> (fun ((a,b),c) -> (a,b,c) |> Bind)
        and parseIdentifier = 
            Parser {
                return! ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            } <?> "Identifier" |>> (toString >> Identifier)
        and parseValue =
            Parser {
                return! ['0'..'9'] |> Seq.toList |> anyOf |> many 1
            } <?> "Value" |>> (List.map string >> List.toSeq >> String.concat "" >> int >> Value)
        and parseFunction  = 
            Parser {
                let pRec   = option ("rec" |> Seq.toList |> allOf)
                let pArrow = "=>" |> Seq.toList |> allOf
                let pArgs = many 1 parseIdentifier 
                let mParams =  ','  |> expect >>. pSpaces
                                    |> separate1By pArgs
                                    |> betweenC ('(',')') 
                return! pRec.>> pSpaces.>>. mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Function" |>> (fun ((Y, args), value) -> match Y with 
                                                            | None   -> (List.concat args, value) |> Function
                                                            | Some _ -> (List.concat args, value) |> Function |> YComb)
        and parseOperation  = 
            Parser {
                let pArgs=  ','  |> expect >>. pSpaces
                                    |> separate1By parseExpression
                                    |> betweenC ('(',')')
                return! parseIdentifier .>>. pArgs
            } <?> "Applicative" |>> Application
        and parseBinary  = 
            Parser {
                let operand = parseIdentifier <|> parseOperation <|> parseValue
                let binOper = ['+';'-';'/';'*';'^'] |> anyOf |>> (string >> Operation.toOp)
                return! operand .>>  pSpaces .>>. binOper .>> pSpaces .>>. operand
            } <?> "Binary Term" |>> (fun ((lhs,op),rhs) -> (lhs,op,rhs) |> Mathematic)
        and parseExpression = 
            Parser {
                let! expr = 
                    choice [    
                        parseLet
                        parseOperation      
                        parseFunction
                        parseBinary
                        parseValue
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
                | YComb(Function(_) as f) -> sprintf "((\\g.(\\y.g (y y)) (\\y.g (y y))) %s)" (emitLambda f)
                | Mathematic(lhs, op, rhs) ->
                    match op  with 
                    | Add -> sprintf "\\g.\\v.((%s g) ((%s g) v))" (emitLambda lhs) (emitLambda rhs)
                    | Mult-> sprintf "\\g.\\v.((%s (%s g)) v)" (emitLambda lhs) (emitLambda rhs)
                    | Exp -> sprintf "(%s %s)" (emitLambda rhs) (emitLambda lhs)
                    | Subs | Div -> failwith "not yet implimented"
                | Value(var) -> 
                    let funcn, varn = "f", "x" //[for i in 1..var -> "f"] |> String.concat "", [for i in 1..var -> "x"] |> String.concat ""
                    let prefix = sprintf "\\%s.\\%s." funcn varn
                    let rec loop n = 
                        match n with 
                        | 0 -> sprintf "%s" varn
                        | 1 -> sprintf "(%s %s)" funcn (loop (n - 1))
                        | _ -> sprintf "(%s %s)" funcn (loop (n - 1))
                    prefix + (loop var)
                | _ -> failwith "Syntax Error : AST incomprehensible"
            emitLambda program
        | Failure _ -> toResult Result
    
    let rec uncompile =
        function 
        | Applicative(f, args) ->
            Application(uncompile f, [uncompile args])
        | Lambda(p, b) -> 
            Function([uncompile p], uncompile b)
        | Atom(name) -> Identifier(name)