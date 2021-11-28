module Abstractor 
    open Interpreter
    open Parsec
    type Thunk = Interpreter.Expression 
    type Statement = 
        | Value             of Literal 
        | Identifier        of string 
        | Bind              of Statement * Statement * Statement 
        | Function          of Statement list * Statement 
        | Application       of Statement * Statement list
        | Unary             of Operation * Statement
        | Binary            of Statement * Operation * Statement
        | Branch            of Statement * Statement * Statement
    and Literal =
        | True | False | Variable of int
    and Operation   =   Add | Subs | Div | Mult | Exp | Or | And | Eq | Lt | Not | Xor | Gt | YComb
                        static member toOp token =
                            match token with 
                            | "*" -> Mult  | "/" -> Div | "^" -> Exp | "+" -> Add
                            | "&" -> And   | "|" -> Or  | "~" -> Not | "!" -> Xor 
                            | "=" -> Eq    | "<" -> Lt  | ">" -> Gt  | "-" -> Subs
                            | "Y" -> YComb | _ -> failwith "Failed Parsing"
    #nowarn "40"
    let parseExpr = 
        let rec parseLet topLevel=
            Parser {
                let [|consumeLet; consumeIn; consumeEnd; consumeEq|] = [|"let"; "in"; "end"; ":="|] |> Array.map (Seq.toList >> allOf)
                return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. parseExpression
                                  .>>  pSpaces .>> (if topLevel then consumeEnd else consumeIn)  .>> pSpaces .>>. parseExpression
            } <?> "Binder" |>> (fun ((a,b),c) -> (a,b,c) |> Bind)
        and parseBrancher =
            Parser {
                let [| consumeIf; consumeThen; consumeElse |] = [|"if"; "then"; "else"|] 
                                                                |> Array.map (Seq.toList >> allOf)
                let pCondition = parseUnary <|> parseBinary <|> parseOperation <|> parseValue <|> parseIdentifier
                return! consumeIf   >>. pSpaces  >>. pCondition      .>> pSpaces 
                    .>> consumeThen .>> pSpaces .>>. parseExpression .>> pSpaces 
                    .>> consumeElse .>> pSpaces .>>. parseExpression
            } <?> "Binder" |>> (fun ((c,t),f) -> (c,t,f) |> Branch)
        and parseIdentifier = 
            ['a'..'z'] |> Seq.toList |> anyOf |> many 1 <?> "Identifier" |>> (toString >> Identifier)
        and parseValue =
            Parser {
                let [| parseT ; parseF |] = [| "true" ;"false"|] 
                                            |> Array.map (Seq.toList >> allOf)
                let parseV     = ['0'..'9'] |> Seq.toList |> anyOf |> many 1
                return! choice [
                    parseT; parseF; parseV
                ]
            } <?> "Value" |>> (List.map string >> List.toSeq >> String.concat "" >> function
                                                                                    | "true" -> True  |> Value
                                                                                    | "false"-> False |> Value
                                                                                    | _ as i -> i |> int |> Variable |> Value)
        and parseFunction  = 
            Parser {
                let pArrow = "=>" |> Seq.toList |> allOf
                let pArgs = many 1 parseIdentifier 
                let mParams =  ','  |> expect >>. pSpaces
                                    |> separate1By pArgs
                                    |> betweenC ('(',')') 
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Function" |>> (fun (args, body) -> (List.concat args, body) |> Function)
        and parseUnary = 
            Parser {
                let uniOper = ['~';'-';'Y'] |> anyOf |>> (string >> Operation.toOp)
                let uniExpr = parseBinary <|> parseOperation <|> parseValue <|> parseFunction <|> parseIdentifier 
                return! uniOper .>> pSpaces .>>. uniExpr
            } <?> "Unary" |>>  Unary
        and parseOperation  = 
            Parser {
                let pArgs=  ',' |> expect >>. pSpaces
                                |> separate1By parseExpression
                                |> betweenC ('(',')')
                return! parseIdentifier .>>. pArgs
            } <?> "Applicative" |>> Application
        and parseBinary  = 
            Parser {
                let operand = parseValue <|> parseUnary <|> parseIdentifier <|> parseOperation
                let binOper = ['+';'-';'/';'*';'^';'|';'&';'=';'<';'>';'!'] |> anyOf |>> (string >> Operation.toOp)
                return! operand .>>  pSpaces .>>. binOper .>> pSpaces .>>. operand
            } <?> "Binary Term" |>> (fun ((lhs,op),rhs) -> (lhs,op,rhs) |> Binary)
        and parseExpression = 
            Parser {
                let! expr = 
                    choice [    
                        parseBrancher
                        parseLet false
                        parseOperation      
                        parseFunction
                        parseBinary
                        parseValue
                        parseUnary
                        parseIdentifier    
                    ] 
                return expr
            } <?> "Expression" 
        parseLet true
    let transpile input = 
        // make this function emit lambda AST instead of parsable strings
        let rec curry =
            function 
            | Function ([_] , _ ) as input -> input
            | Function (h::t,body) -> Function ([h], curry <| Function(t, body))
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
                    wrap operation args
                | Identifier(name) -> name
                | Unary(Op, rhs) -> 
                    match Op with 
                    | Not -> sprintf "(\\_g.((_g %s) %s) %s)" (emitLambda (Value False)) (emitLambda (Value True)) (emitLambda rhs)
                    | YComb ->
                        let printfY = sprintf "(\\_g.(\\_y.(_g (_y _y)) \\_y.(_g (_y _y))) %s)"
                        match rhs with
                        | Function _ as f -> printfY (emitLambda f)
                        | _ -> failwith "YComb only applies to Lambdas/Functions"
                    | _ -> failwith "Unary operator not supported"
                | Branch(cond,tClause, fClause) as t -> 
                    let IfThenElse = sprintf "(\\_c.(\\_h.(\\_l.((_c _h) _l) %s) %s) %s)"
                    IfThenElse (emitLambda fClause) (emitLambda tClause) (emitLambda cond) 
                | Binary(lhs, op, rhs) ->
                    let isZero = sprintf "(\\_z.((_z \\_w.%s) %s) %s)" (emitLambda (Value False)) (emitLambda (Value True))
                    let predec = sprintf "(\\_d.\\_h.\\_w.(((_d \\_g.\\_h.(_h (_g _h))) \\_u._w) \\_u._u) %s)"
                    match op  with 
                    | Add -> sprintf "\\_g.\\_v.((%s _g) ((%s _g) _v))" (emitLambda lhs) (emitLambda rhs)
                    | Mult-> sprintf "\\_g.\\_v.((%s (%s _g)) _v)" (emitLambda lhs) (emitLambda rhs)
                    | Exp -> sprintf "(%s %s)" (emitLambda rhs) (emitLambda lhs)
                    | And -> sprintf "((\\_g.\\_v.((_g _v) %s) %s) %s)" (emitLambda (Value False)) (emitLambda lhs) (emitLambda rhs)
                    | Or  -> sprintf "((\\_g.\\_v.((_g %s) _v) %s) %s)" (emitLambda (Value True)) (emitLambda lhs) (emitLambda rhs)
                    | Subs-> sprintf "((\\_g.\\_v.(_v %s) %s) %s)" (predec "_g") (emitLambda lhs) (emitLambda rhs)
                    | Lt  -> isZero (emitLambda (Binary(lhs, Subs, rhs))) | Gt  -> emitLambda (Binary(rhs, Lt, lhs))
                    | Eq  -> emitLambda (Binary(Binary(lhs, Lt, rhs), And, Binary(lhs, Gt, rhs)))
                    | Xor -> sprintf "((\\_g.\\_v.((_g %s) _v) %s) %s)" (emitLambda (Unary(Not, Identifier("_v")))) (emitLambda lhs) (emitLambda rhs)
                | Value(var) -> 
                    match var with 
                    | True -> "\\_a.\\_b._a"
                    | False -> "\\_a.\\_b._b"
                    | Variable(var) ->
                        let funcn, varn = "_v", "_x" //[for i in 1..var -> "f"] |> String.concat "", [for i in 1..var -> "x"] |> String.concat ""
                        let rec loop n = 
                            match n with 
                            | 0 -> sprintf "%s" varn
                            | _ -> sprintf "(%s %s)" funcn (loop (n - 1))
                        sprintf "\\%s.\\%s.%s" funcn varn (loop var)
                | _ -> failwith "Syntax Error : AST incomprehensible"
            emitLambda program
        | Failure _ -> toResult Result
    
    let rec uncompile =
        function 
        | Applicative(f, args) -> Application(uncompile f, [uncompile args])
        | Lambda(p, b) -> Function([uncompile p], uncompile b)
        | Atom(name) -> Identifier(name)
