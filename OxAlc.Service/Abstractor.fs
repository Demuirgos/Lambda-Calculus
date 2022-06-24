module Abstractor 
    open Interpreter
    open Parsec
    open System.IO
    open System.Text.RegularExpressions
    type Thunk = Interpreter.Expression 
    exception Error of string
    type Statement = 
        | Value             of Literal 
        | Identifier        of string 
        | Bind              of Statement * Statement * Statement 
        | Function          of Parameter list * Statement 
        | Application       of Statement * Statement list
        | Unary             of Operation * Statement
        | Binary            of Statement * Operation * Statement
        | Branch            of Statement * Statement * Statement
        | Compound          of Statement * (Statement * Statement) list   
        | Context           of Statement list * Statement
    and Literal =
        | Hole | True | False 
        | Variable of int 
        | String of List<char> 
        | List of List<Statement>  
        | Record of Map<string,Statement>
    and Parameter = 
        | Argument of Statement | Pattern of Statement * Statement
    and Operation   =   Cons |Add | Subs | Div | Mult | Exp | Or | And | Eq | Lt | Not | Xor | Gt | YComb | Custom of string
                        static member toOp tokens =
                            match tokens with 
                            | ['*'] -> Mult  | ['/'] -> Div  | ['^'] -> Exp | ['+'] -> Add
                            | ['&'] -> And   | ['|'] -> Or   | ['~'] -> Not | ['!'] -> Xor 
                            | ['='] -> Eq    | ['<'] -> Lt   | ['>'] -> Gt  | ['-'] -> Subs
                            | [ 'Y'] -> YComb | ['@'] -> Cons | _ -> Custom ( tokens |> List.map string |> String.concat "") 
    and Backend = LCR | LLVM | MSIL

    let (|Comment|_|) pattern input =
        let regex = Regex.Match(input, pattern)
        if regex.Success then Some([ for m in regex.Captures do m ])
        else None

    #nowarn "40"
    let parseExpr = 
        let rec parseLet topLevel =
                Parser {
                    let [|consumeLet; consumeIn; consumeEnd; consumeEq|] = [|"let"; "in"; "end"; ":="|] |> Array.map parserWord
                    return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. parseExpression
                                    .>>  pSpaces .>> (if topLevel then consumeEnd else consumeIn)  .>> pSpaces .>>. parseExpression
                } <?> "Binder" |>> (fun ((a,b),c) -> (a,b,c) |> Bind)
        and parseCompound =
                Parser {
                    let [|consumeWhere; consumeEq; consumeAnd|] = [|"where"; ":="; "and"|] |> Array.map parserWord
                    let parseExpression =   choice [    
                                                parseBrancher; parseFunction; parseBinary; parseOperation; parseValue; parseUnary; parseIdentifier    
                                            ] 
                    let parseBinders = parseIdentifier .>> pSpaces  .>> consumeEq     .>> pSpaces .>>. parseExpression .>> option (pSpaces .>> consumeAnd .>> pSpaces )
                                        |> many 1
                    return! parseExpression .>> pSpaces .>> consumeWhere .>> pSpaces .>>. parseBinders  
                } <?> "Binder" |>> Compound
        and parseBrancher =
            let parseTernary =
                Parser {
                    let [| consumeIf; consumeElse |] = [|'?'; ':'|] 
                                                        |>  Array.map expect
                    let pCondition = choice [parseUnary; parseBinary; parseOperation; parseValue; parseIdentifier]
                    return!  pCondition .>> pSpaces .>> consumeIf   .>> pSpaces .>>. parseExpression 
                                        .>> pSpaces .>> consumeElse .>> pSpaces .>>. parseExpression 
                } |>> (fun ((c,t),f) -> (c,t,f) |> Branch)
            let parseIf = 
                Parser {
                    let [| consumeIf; consumeThen; consumeElse |] = [|"if"; "then"; "else"|] 
                                                                    |> Array.map parserWord
                    let pCondition = choice [parseTernary; parseUnary; parseBinary; parseOperation; parseValue; parseIdentifier]
                    return! consumeIf   >>. pSpaces  >>. pCondition      .>> pSpaces 
                        .>> consumeThen .>> pSpaces .>>. parseExpression .>> pSpaces 
                        .>> consumeElse .>> pSpaces .>>. parseExpression
                } |>> (fun ((c,t),f) -> (c,t,f) |> Branch)
            (parseTernary <|> parseIf) <?> "Brancher" 
        and parseIdentifier = 
            (['a'..'z']@['+';'-';'/';'*';'^';'|';'&';'=';'<';'>';'!';'@']@['0'..'9']) 
            |> Seq.toList 
            |> anyOf |> many 1 
            <?> "Identifier" |>> (toString >> Identifier)
        and parseValue =
            let rec parseSimple = 
                Parser {
                    let [| parseT ; parseF |] = [| "true" ;"false"|] 
                                                |> Array.map parserWord
                    let parseV     = ['0'..'9'] |> Seq.toList |> anyOf |> many 1
                    return! choice [
                        parseT; parseF; parseV
                    ]
                } <?> "Value" |>> (List.map string >> List.toSeq >> String.concat "" >> function
                                                                                    | "true" -> True  |> Value
                                                                                    | "false"-> False |> Value
                                                                                    | _ as i -> i |> int |> Variable |> Value)
            and parseList = 
                Parser {
                    let pElems= ',' |> expect >>. pSpaces
                                    |> separateBy 0 parseExpression
                                    |> betweenC ('[',']')
                    return! pElems
                } <?> "List Expr" |>> (List >> Value) 
            and parseString =
                Parser {
                    let validChars = ([' ']@['a'..'z']@['+';'-';'/';'*';'^';'|';'&';'=';'<';'>';'!';'@']@['0'..'9']) 
                    return! validChars 
                                |> anyOf |> many 0
                                |> betweenC ('"','"')
                } <?> "String Expr" |>> (String >> Value)
            and parseHole = 
                Parser {
                    return! expect '?' |> many 1
                } <?> "Hole Expr" |>> (fun _ -> Hole |> Value)
            parseSimple <|> parseList <|> parseString <|> parseHole
        and parseFunction  = 
            Parser {
                let [pArrow ; consumeDec] = ["=>"; "::"] |> List.map parserWord
                let pArg  = parseIdentifier 
                            |>> Argument
                let pPatt = parseIdentifier .>> pSpaces .>> consumeDec .>> pSpaces .>>. parseIdentifier
                            |>>  Pattern
                let mParam  = pPatt <|> pArg |>> List.singleton
                let mParams =  ','  |> expect >>. pSpaces
                                    |> separateBy 1 (pPatt <|> pArg)
                                    |> betweenC ('(',')')
                                <|> mParam
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Function" |>> Function
        and parseUnary = 
            Parser {
                let uniOper = ['~';'-';'Y'] |> anyOf |> many 1 |>> Operation.toOp
                              <|> ( "rec" |> Seq.toList 
                                    |> allOf |>> fun _ -> YComb)
                let uniExpr =   (betweenC ('(',')') parseUnary)  
                                <|> choice [ parseBinary; parseOperation; parseValue; parseFunction; parseIdentifier] 
                return! uniOper .>> pSpaces .>>. uniExpr
            } <?> "Unary" |>>  Unary
        and parseOperation  = 
            Parser {
                let pArgs=  ',' |> expect >>. pSpaces
                                |> separateBy 1 parseExpression
                                |> betweenC ('(',')')
                return! parseIdentifier .>>. pArgs
            } <?> "Applicative" |>> Application
        and parseBinary  = 
            Parser {
                let operand =   (betweenC ('(',')') parseBinary)   
                                <|> choice [ parseUnary; parseOperation; parseValue; parseIdentifier] 
                let binOper =   ['+';'-';'/';'*';'^';'|';'&';'=';'<';'>';'!';'@'] 
                                |> anyOf |> many 1 
                                |>> Operation.toOp
                return! operand .>>  pSpaces .>>. binOper .>> pSpaces .>>. operand
            } <?> "Binary Term" |>> (fun ((lhs,op),rhs) -> (lhs,op,rhs) |> Binary)
        and parseInclude =
            Parser {
                let parsefiles = ',' |> expect >>. pSpaces
                                |> separateBy 0 parseIdentifier
                                |> betweenC ('[',']')
                let [|consumeInclude; consumeFor|]  = [|"include"; "for"|]  |> Array.map parserWord
                return! consumeInclude >>. pSpaces >>. parsefiles .>> pSpaces .>> consumeFor .>> pSpaces .>>. parseExpression
            } <?> "Include" |>> Context
        and parseExpression = 
            Parser {
                let! expr = 
                    choice [
                        parseInclude    
                        parseBrancher
                        parseLet false
                        parseCompound
                        parseFunction
                        parseBinary
                        parseOperation      
                        parseValue
                        parseUnary
                        parseIdentifier    
                    ] 
                return expr
            } <?> "Expression" 
        parseLet true

    let transpile backend rawInput  = 
        match backend with 
        | LCR -> 
            let input = 
                match rawInput with 
                | Comment "\(\*.*?\*\)" comments ->
                    let rec removeComments (str:string) (comments: Capture list) =  
                        match comments with
                        | [] -> str
                        | h::t -> removeComments (str.Remove(h.Index, h.Length)) t
                    removeComments rawInput comments
                | _ -> rawInput
            let rec curry =
                function 
                | Function ([_] , _ ) as input -> input
                | Function ((h::t),body) -> Function ([h], curry <| Function(t, body))
                | _ -> failwith "Expression cannot be curried"

            let parseExp arg = (fromStr arg, parseExpr) ||> run 
            let Result = parseExp input
            match Result with
            | Success (program,r) -> 
                let toSyntaxTree = parse     >> (function Success(code,_) -> code) >> 
                                interpret >> (function Ok (program)    -> program)
                let rec emitLambda= function
                    | Context(files, program) ->
                        let filesContents = files |> List.map (function Identifier(path) 
                                                                                                    -> path |> (sprintf "%s.oxalc") 
                                                                                                            |> File.ReadAllText 
                                                                                                            |> parseExp)
                        let replaceLastNode expr = 
                            function Bind(_, defs, _) -> 
                                let rec loop target = 
                                    match target with 
                                    | Bind (n, e, Value(Hole)) -> Bind(n ,e, expr)
                                    | Bind (n, e, v) -> Bind(n,e, loop v)
                                    | _ as lib-> failwithf "%A invalid format for Library" lib
                                loop defs 
                        let rec wrapFile filesASTs exprAcc =  
                            match List.rev filesASTs with 
                            | [] -> exprAcc
                            | h::t -> 
                                match h with 
                                | Success(expr, _) -> wrapFile t (replaceLastNode exprAcc expr)
                                | _ as error-> failwithf "%A" error
                        let r = (wrapFile filesContents program)  
                        emitLambda r
                    | Bind(name, expr, value) ->
                        Applicative(Lambda(emitLambda name, emitLambda value), emitLambda expr)
                    | Compound(expr, binds) as e -> 
                        let rec emitBinds binds = 
                            match binds with 
                            | [] -> emitLambda expr
                            | bind::binds ->
                                let (id, value) = bind 
                                Applicative(Lambda(emitLambda id, emitBinds binds), emitLambda value)
                        emitBinds binds
                    | Function _ as f->
                        let emitFunction (Function([param], body)) = match param with 
                            | Argument(a)    -> Lambda(emitLambda a, emitLambda body)
                            | Pattern (h, t) as p ->
                                let head = Lambda(Atom "_l", Applicative(Atom "_l", emitLambda(Value True )))
                                let tail = Lambda(Atom "_l", Applicative(Atom "_l", emitLambda(Value False)))
                                Lambda(Atom "_l", Applicative(Lambda(emitLambda h, Applicative(Lambda(emitLambda t, emitLambda body), Applicative(tail, Atom "_l"))), Applicative(head, Atom "_l")))
                        f |> curry |> emitFunction
                    | Application(expr, args) as a ->
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
                        | Custom(token) -> emitLambda(Application(Identifier token, [lhs; rhs]))
                        | Cons -> emitLambda (Value (List [lhs; rhs]))
                        | Div | Not | YComb -> failwith "Not Implemented"
                    | Value(var) -> 
                        match var with 
                        | List(elems)-> 
                            let cons = Lambda(Atom "_h", Lambda(Atom "_t", Lambda(Atom "_s", Applicative(Applicative(Atom "_s", Atom "_h"), Atom "_t"))))
                            let empty= Lambda(Atom "_l", Applicative(Atom "_l", Lambda(Atom "_h", Lambda(Atom "_t", emitLambda(Value False)))))
                            let nil  = Lambda(Atom "_l", Applicative(Atom "_l", emitLambda(Value True )))
                            let rec emitList = function 
                                | []   -> nil
                                | h::t -> Applicative(Applicative(cons, emitLambda h), emitList t)
                            emitList elems
                        | True -> Lambda(Atom "_a", Lambda(Atom "_b", Atom "_a"))
                        | False -> Lambda(Atom "_a", Lambda(Atom "_b", Atom "_b"))
                        | Variable(var) ->
                            let funcn, varn = "_a", "_b"
                            let rec loop n = 
                                match n with 
                                | 0 -> Atom varn
                                | _ -> Applicative(Atom funcn, (loop (n - 1)))
                            Lambda(Atom funcn, Lambda(Atom varn, loop var))
                        | _ as v -> failwithf "%A is not supported by Lambda-Calculus" v 
                Success (emitLambda program, r)
            | Failure(l, e, idx) -> Failure(l,e,idx)
        | _ -> failwith "Backend not supported : Yet" 

    let rec uncompile =
        function 
        | Applicative(f, args) -> Application(uncompile f, [uncompile args])
        | Lambda(p, b) -> Function([Argument (uncompile p)], uncompile b)
        | Atom(name) -> Identifier(name)

    let parse text = Parsec.Parser.run (fromStr text) parseExpr


// let library := 
// 
// 
// 
// end library

