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
        | Success (program,r) -> 
            let toSyntaxTree = parse     >> (function Success(code,_) -> code) >> 
                               interpret >> (function Ok(program)     -> program)
            let rec emitLambda= function
                | Bind(name, expr, value) ->
                    Applicative(Lambda(emitLambda name, emitLambda value), emitLambda expr)
                | Function(parameters, _) as f->
                    let emit = function | Function([param], expr) -> Lambda(emitLambda param, emitLambda expr)
                                        | _ -> failwithf "%A is not a lambda, transpiling failed" (nameof f)
                    match parameters with 
                    | [] | [_] -> emit f
                    | _ -> emit (curry f)
                | Application(expr, args) ->
                    let operation = emitLambda expr
                    let rec wrap op = function
                        | [] -> op
                        | h::t -> wrap (Applicative(op, (emitLambda h))) t
                    wrap operation args
                | Identifier(name) -> Atom name
                | Unary(Op, rhs) -> 
                    match Op with 
                    | Not -> Applicative(Applicative(emitLambda rhs, emitLambda (Value False)), emitLambda (Value True))
                    | YComb ->
                        let Y = "\\_g.(\\_y.(_g (_y _y)) \\_y.(_g (_y _y)))" |> toSyntaxTree
                        printfn "%A" Y
                        Applicative(Y, (emitLambda rhs))
                    | _ -> failwith "Unary operator not supported" 
                | Branch(cond,tClause, fClause) as t -> 
                    Applicative (Applicative(emitLambda cond, emitLambda tClause), emitLambda fClause)
                | Binary(lhs, op, rhs) ->
                    let isZero arg = Applicative(Applicative(arg,Lambda(Atom "_w", emitLambda (Value False))), emitLambda (Value True))
                    let predec = "\\_n.\\_f.\\_x.(((_n \\_g.\\_h.(_h (_g _f))) \\_u._x) \\_u._u)" |> toSyntaxTree
                    match op  with 
                    | Add -> Lambda(Atom "_g", Lambda(Atom "_v", Applicative(Applicative(emitLambda lhs, Atom "_g"), Applicative(Applicative(emitLambda rhs, Atom "_g"), Atom "_v"))))
                    | Mult-> Lambda(Atom "_g", Lambda(Atom "_v", Applicative(Applicative(emitLambda lhs, Applicative(emitLambda rhs, Atom "_g")), Atom "_v")))
                    | Exp -> Applicative(emitLambda rhs, emitLambda lhs)
                    | And -> Applicative(Applicative(emitLambda rhs, emitLambda lhs), emitLambda (Value False))
                    | Or  -> Applicative(Applicative(emitLambda rhs, emitLambda (Value False)), emitLambda lhs)
                    | Subs-> Applicative(Applicative(emitLambda rhs, predec), emitLambda lhs)
                    | Lt  -> isZero (emitLambda (Binary(lhs, Subs, rhs))) | Gt  -> emitLambda (Binary(rhs, Lt, lhs))
                    | Eq  -> emitLambda (Binary(Binary(lhs, Lt, rhs), And, Binary(lhs, Gt, rhs)))
                    | Xor -> Applicative(Applicative(emitLambda rhs, emitLambda (Unary(Not, lhs))), emitLambda lhs)
                 | Value(var) -> 
                    match var with 
                    | True -> Lambda(Atom "_a", Lambda(Atom "_b", Atom "_a"))
                    | False -> Lambda(Atom "_a", Lambda(Atom "_b", Atom "_b"))
                    | Variable(var) ->
                        let funcn, varn = "_a", "_b" //[for i in 1..var -> "f"] |> String.concat "", [for i in 1..var -> "x"] |> String.concat ""
                        let rec loop n = 
                            match n with 
                            | 0 -> Atom varn
                            | _ -> Applicative(Atom funcn, (loop (n - 1)))
                        Lambda(Atom funcn, Lambda(Atom varn, loop var))
            let result = emitLambda program
            Success (result, r)
        | Failure _ -> failwith "Syntax Error : Syntax incomprehensible"
    
    let rec uncompile =
        function 
        | Applicative(f, args) -> Application(uncompile f, [uncompile args])
        | Lambda(p, b) -> Function([uncompile p], uncompile b)
        | Atom(name) -> Identifier(name)

    let parse text = Parsec.Parser.run (fromStr text) parseExpr