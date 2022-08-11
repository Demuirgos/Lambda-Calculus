module OxalcParser 
    open System
    open Parsec
    open System.IO
    open Typedefinitions
    open System.Text.RegularExpressions

    let (|Comment|_|) pattern input =
        let regex = Regex.Match(input, pattern)
        if regex.Success then Some(Seq.toList regex.Captures)
        else None

    #nowarn "40"
    let parseExpr = 
        let parseType = 
            let rec parseAtom = 
                Parser {
                    return! option ([yield! ['a' .. 'z'];] |> Seq.toList |> anyOf |> many 1) 
                } <?> "Atom" 
                    |>> function 
                        | Some(value) -> value |> toString |> (fun str -> str.Trim()) |> Atom
                        | None        -> Atom String.Empty
            and parseArrow = 
                Parser {
                    let operand =   (betweenC ('(',')') parseArrow) <|> parseAtom 
                    let binOper =  parseWord "->"
                    return! operand .>>  pSpaces .>> binOper .>> pSpaces .>>. operand
                } <?> "Arrow" |>> Arrow
            parseArrow <|> parseAtom 
        let rec parseLet topLevel =
                let mapper = fun (((var, var_t),b),c) -> (var, var_t, b, c)
                Parser {
                    let [|consumeLet; consumeIn; consumeEnd; consumeTyper;  consumeEq|] = [|"let"; "in"; "end"; ":"; "="|] |> Array.map parseWord
                    return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeTyper .>>. parseType .>> consumeEq .>> pSpaces .>>. parseExpression
                                    .>>  pSpaces .>> (if topLevel then consumeEnd else consumeIn)  .>> pSpaces .>>. parseExpression
                } <?> "Binder" |>> ( mapper >> Bind )
        and parseCompound =
                Parser {
                    let [|consumeWhere; consumeTyper; consumeEq; consumeAnd|] = [|"where"; ":"; "="; "and"|] |> Array.map parseWord
                    let parseExpression =   choice [    
                                                parseBrancher; parseFunction; parseBinary; parseOperation; parseValue; parseUnary; parseIdentifier    
                                            ] 
                    let parseBinders = parseIdentifier .>> pSpaces  .>> consumeTyper .>>. parseType .>> consumeEq .>> pSpaces .>>. parseExpression .>> option (pSpaces .>> consumeAnd .>> pSpaces )
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
                                                                    |> Array.map parseWord
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
                                                |> Array.map parseWord
                    let parseV     = ['0'..'9'] |> Seq.toList |> anyOf |> many 1
                    return! choice [
                        parseT; parseF; parseV
                    ]
                } <?> "Value" |>> (List.map string >> List.toSeq >> String.concat "" >> function
                                                                                    | "true" -> Bool true |> Value
                                                                                    | "false"-> Bool false |> Value
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
                let [pArrow ; consumeTyper] = ["=>"; ":"] |> List.map parseWord
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
                let [|consumeInclude; consumeFor|]  = [|"include"; "for"|]  |> Array.map parseWord
                return! consumeInclude >>. pSpaces >>. parsefiles .>> pSpaces .>> consumeFor .>> pSpaces .>>. parseExpression
            } <?> "Include" |>> Context
        and parseTypeDef = 
            let rec parseType =  
                let rec parseSum = 
                    Parser {
                        let pUnion = parseWord "|"
                        return! parseType .>> pSpaces .>> pUnion .>> pSpaces .>>. parseType
                    } <?> "Union" |>> Union
                and parseMult = 
                    Parser {
                        let pIntersection = parseWord "&"
                        return! parseType .>> pSpaces .>> pIntersection .>> pSpaces .>>. parseType  
                    } <?> "Intersection" |>> Intersection
                and parseExp = 
                    Parser {
                        let pExponent = parseWord "^"
                        return! parseType .>> pSpaces .>> pExponent .>> pSpaces .>>. parseType  
                    } <?> "Exponent" |>> Exponent
                parseSum <|> parseMult <|> parseExp
            Parser {        
                let [typeDecl; consumeEq] = ["type"; "<="] |> List.map parseWord
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

    let parse text = Parsec.Parser.run (fromStr text) parseExpr