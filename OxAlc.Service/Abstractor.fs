module Abstractor 
    open Interpreter
    open Typechecker
    open Typedefinitions
    open Parsec
    open System.IO
    open System.Text.RegularExpressions

    let (|Comment|_|) pattern input =
        let regex = Regex.Match(input, pattern)
        if regex.Success then Some(Seq.toList regex.Captures)
        else None

    #nowarn "40"
    let parseExpr = 
        let rec parseLet topLevel =
                let mapper = fun (((var, var_t),b),c) -> (var, var_t, b, c)
                Parser {
                    let [|consumeLet; consumeIn; consumeEnd; consumeTyper;  consumeEq|] = [|"let"; "in"; "end"; ":"; "="|] |> Array.map parserWord
                    return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeTyper .>>. parseType .>> consumeEq .>> pSpaces .>>. parseExpression
                                    .>>  pSpaces .>> (if topLevel then consumeEnd else consumeIn)  .>> pSpaces .>>. parseExpression
                } <?> "Binder" |>> ( mapper >> Bind )
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
                let mapper = fun ((c,t),f) -> (c,t,f)
                Parser {
                    let [| consumeIf; consumeElse |] = [|'?'; ':'|] 
                                                        |>  Array.map expect
                    let pCondition = choice [parseUnary; parseBinary; parseOperation; parseValue; parseIdentifier]
                    return!  pCondition .>> pSpaces .>> consumeIf   .>> pSpaces .>>. parseExpression 
                                        .>> pSpaces .>> consumeElse .>> pSpaces .>>. parseExpression 
                } |>> ( mapper >> Branch)
            let parseIf = 
                let mapper = fun ((c,t),f) -> (c,t,f)
                Parser {
                    let [| consumeIf; consumeThen; consumeElse |] = [|"if"; "then"; "else"|] 
                                                                    |> Array.map parserWord
                    let pCondition = choice [parseTernary; parseUnary; parseBinary; parseOperation; parseValue; parseIdentifier]
                    return! consumeIf   >>. pSpaces  >>. pCondition      .>> pSpaces 
                        .>> consumeThen .>> pSpaces .>>. parseExpression .>> pSpaces 
                        .>> consumeElse .>> pSpaces .>>. parseExpression
                } |>> ( mapper >> Branch )
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
                let [pArrow ; consumeTyper] = ["=>"; ":"] |> List.map parserWord
                let pArg  = parseIdentifier .>> consumeTyper .>>. parseType
                let mParams =  ','  |> expect >>. pSpaces
                                    |> separateBy 1  pArg
                                    |> betweenC ('(',')')
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
        and parseTypeDef = 
            let rec parseType =  
                let rec parseSum = 
                    Parser {
                        let pUnion = parserWord "|"
                        return! parseType .>> pSpaces .>> pUnion .>> pSpaces .>>. parseType
                    } <?> "Union" |>> Union
                and parseMult = 
                    Parser {
                        let pIntersection = parserWord "&"
                        return! parseType .>> pSpaces .>> pIntersection .>> pSpaces .>>. parseType  
                    } <?> "Intersection" |>> Intersection
                and parseExp = 
                    Parser {
                        let pExponent = parserWord "^"
                        return! parseType .>> pSpaces .>> pExponent .>> pSpaces .>>. parseType  
                    } <?> "Exponent" |>> Exponent
                parseSum <|> parseMult <|> parseExp
            Parser {        
                let [typeDecl; consumeEq] = ["type"; "<="] |> List.map parserWord
                return! typeDecl >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. parseType
            } <?> "Type Definition" |>> Typedefinition
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
                        let filesContents = files  |> List.map (function Identifier(path) 
                                                                                                    -> path |> (sprintf "%s.oxalc") 
                                                                                                            |> File.ReadAllText 
                                                                                                            |> parseExp)
                        let all xs = 
                            let folder = fun state next -> 
                                match (state, next) with 
                                | (Ok ys, Success (n, s)) -> ys |> List.append [ n ] |> Ok
                                | _  -> Error "File import Failed"
                            Seq.fold folder (Ok []) xs
                        let rec wrapFiles filesAst program = 
                            match filesAst with 
                            | []   -> program
                            | Bind(n, _t , f, e)::t ->
                                Bind(n, _t, f, Application(n, [wrapFiles t program]))
                        match all filesContents with
                        | Ok (Asts) ->   
                            let r = wrapFiles (Asts) program
                            printfn "%A" r
                            emitLambda r
                        | Error(msg) -> failwith msg
                    | Bind(name,type_t,  expr, value) ->
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
                            | (n, t)    -> Lambda(emitLambda n, emitLambda body)
                        f |> curry |> emitFunction
                    | Application(expr, args) as a ->
                        let operation = emitLambda expr
                        let rec wrap op = function
                            | [] -> op
                            | h::t -> wrap (Applicative(op, (emitLambda h))) t
                        wrap operation args
                    | Identifier(name) -> Term name
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
                        let isZero arg = Applicative(Applicative(arg,Lambda(Term "_w", emitLambda (Value False))), emitLambda (Value True))
                        let predec = "\\_n.\\_f.\\_x.(((_n \\_g.\\_h.(_h (_g _f))) \\_u._x) \\_u._u)" |> toSyntaxTree
                        match op  with 
                        | Add -> Lambda(Term "_g", Lambda(Term "_v", Applicative(Applicative(emitLambda lhs, Term "_g"), Applicative(Applicative(emitLambda rhs, Term "_g"), Term "_v"))))
                        | Mult-> Lambda(Term "_g", Lambda(Term "_v", Applicative(Applicative(emitLambda lhs, Applicative(emitLambda rhs, Term "_g")), Term "_v")))
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
                            let cons = Lambda(Term "_h", Lambda(Term "_t", Lambda(Term "_s", Applicative(Applicative(Term "_s", Term "_h"), Term "_t"))))
                            let empty= Lambda(Term "_l", Applicative(Term "_l", Lambda(Term "_h", Lambda(Term "_t", emitLambda(Value False)))))
                            let nil  = Lambda(Term "_l", Applicative(Term "_l", emitLambda(Value True )))
                            let rec emitList = function 
                                | []   -> nil
                                | h::t -> Applicative(Applicative(cons, emitLambda h), emitList t)
                            emitList elems
                        | True -> Lambda(Term "_a", Lambda(Term "_b", Term "_a"))
                        | False -> Lambda(Term "_a", Lambda(Term "_b", Term "_b"))
                        | Variable(var) ->
                            let funcn, varn = "_a", "_b"
                            let rec loop n = 
                                match n with 
                                | 0 -> Term varn
                                | _ -> Applicative(Term funcn, (loop (n - 1)))
                            Lambda(Term funcn, Lambda(Term varn, loop var))
                        | _ as v -> failwithf "%A is not supported by Lambda-Calculus" v 
                Success (emitLambda program, r)
            | Failure(l, e, idx) -> Failure(l,e,idx)
        | _ -> failwith "Backend not supported : Yet" 

    // let rec uncompile =
    //     function 
    //     | Applicative(f, args) -> Application(uncompile f, [uncompile args])
    //     | Lambda(p, b) -> Function([(uncompile p)], uncompile b)
    //     | Term(name) -> Identifier(name), Atom("Erased")

    let parse text = Parsec.Parser.run (fromStr text) parseExpr


// let library := 
// 
// 
// 
// end library

