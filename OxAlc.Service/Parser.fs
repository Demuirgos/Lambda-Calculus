module OxalcParser 
    open System
    open Parsec
    open System.IO
    open Typedefinitions
    open System.Text.RegularExpressions

    let (|Comment|_|) input =
        let pattern = "\(\*.*?\*\)" 
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
                    let operand =   (betweenC ('(', ')') parseArrow) <|> parseAtom 
                    let binOper =  parseWord "->"
                    return! operand .>>  pSpaces .>> binOper .>> pSpaces .>>. operand
                } <?> "Arrow" |>> Arrow
            parseArrow <|> parseAtom 
        let rec parseLet topLevel =
                let mapper = fun (((var, var_t),b),c) -> (var, var_t, b, c)
                Parser {
                    let [|consumeLet; consumeIn; consumeEnd; consumeTyper;  consumeEq|] = [|"let"; "in"; "end"; ":"; "="|] |> Array.map parseWord
                    return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeTyper .>>. parseType .>> consumeEq .>> pSpaces .>>. parseExpression
                                      .>>  pSpaces .>> (if topLevel then consumeEnd else consumeIn) .>>  pSpaces   .>>. parseExpression
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
            let mapper = fun ((c,t),f) -> (c,t,f)
            Parser {
                let [| consumeIf; consumeThen; consumeElse |] = [|"if"; "then"; "else"|] 
                                                                |> Array.map parseWord
                let pCondition = choice [parseUnary; parseBinary; parseOperation; parseValue; parseIdentifier]
                return! consumeIf   >>. pSpaces  >>. pCondition      .>> pSpaces 
                    .>> consumeThen .>> pSpaces .>>. parseExpression .>> pSpaces 
                    .>> consumeElse .>> pSpaces .>>. parseExpression
            } |>> ( mapper >> Branch )  <?> "Brancher" 
        and parseIdentifier = 
            '_'::printable
            |> Seq.toList 
            |> anyOf |> many 1 
            <?> "Identifier" |>> (toString >> Identifier)
        and parseValue =
            let rec parseSimple = 
                Parser {
                    let [| parseT ; parseF |] = [| "true" ;"false"|] 
                                                |> Array.map parseWord
                    let parseV     = digits |> Seq.toList |> anyOf |> many 1
                    return! choice [
                        parseT; parseF; parseV
                    ]
                } <?> "Value" |>> (List.map string >> List.toSeq >> String.concat "" >> function
                                                                                    | "true" -> Bool true |> Value
                                                                                    | "false"-> Bool false |> Value
                                                                                    | _ as i -> i |> int |> Variable |> Value)
            and parseList = 
                Parser {
                    let pElems= (pSpaces >>.  (',' |> expect) >>. pSpaces)
                                    |> separateBy 0 parseExpression
                                    |> betweenC ('[', ']')
                    return! pElems
                } <?> "List Expr" |>> (List >> Value) 
            and parseString =
                Parser {
                    let validChars = (' ' :: printable) 
                    return! validChars 
                                |> anyOf |> many 0
                                |> betweenC ('"', '"') 
                } <?> "String Expr" |>> (String >> Value)
            parseSimple <|> parseList <|> parseString
        and parseFunction  = 
            Parser {
                let [pArrow ; consumeTyper] = ["=>"; ":"] |> List.map parseWord
                let pArg  = parseIdentifier .>> consumeTyper .>>. parseType
                let mParams =  (pSpaces >>. (','  |> expect) >>. pSpaces)
                                    |> separateBy 1  pArg
                                    |> betweenC ('(', ')')
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Function" |>> Function
        and parseUnary = 
            Parser {
                let uniOper = ['~';'-';'Y'] |> anyOf |> many 1 |>> Operation.toOp
                              <|> ( "rec" |> Seq.toList 
                                    |> allOf |>> fun _ -> YComb)
                let uniExpr =   (betweenC ('(', ')') parseUnary)  
                                <|> choice [ parseBinary; parseOperation; parseValue; parseFunction; parseIdentifier] 
                return! uniOper .>> pSpaces .>>. uniExpr
            } <?> "Unary" |>>  Unary
        and parseOperation  = 
            let legacyParser = 
                Parser {
                    let pArgs=  (pSpaces >>. (',' |> expect) >>. pSpaces)
                                    |> separateBy 1 parseExpression
                                    |> betweenC ('(', ')') 
                    let pOpr = parseIdentifier <|> (betweenC ('(', ')') parseExpression)
                    return! pOpr .>>. pArgs
                } 
            (legacyParser) <?> "Applicative" |>> Application
        and parseBinary  = 
            Parser {
                let operand =   (betweenC ('(', ')') parseBinary)   
                                <|> choice [ parseUnary; parseOperation; parseValue; parseIdentifier] 
                let binOper = symbols
                                |> anyOf |> many 1 
                                |>> Operation.toOp
                return! operand .>>  pSpaces .>>. binOper .>> pSpaces .>>. operand
            } <?> "Binary Term" |>> (fun ((lhs,op),rhs) -> (lhs,op,rhs) |> Binary)
        and parseInclude =
            Parser {
                let parsefiles = (pSpaces >>. (',' |> expect) >>. pSpaces)
                                |> separateBy 0 parseIdentifier
                                |> betweenC ('[', ']')
                let [|consumeInclude; consumeFor|]  = [|"include"; "for"|]  |> Array.map parseWord
                return! consumeInclude >>. pSpaces >>. parsefiles .>> pSpaces .>> consumeFor .>> pSpaces .>>. parseExpression
            } <?> "Include" |>> Context
        and parseTypeDef = 
            let rec parseAlgebraicType =  
                let rec parseUnit = 
                    parseType <?> "Atom" |>> Const
                and parseSum = 
                    Parser {
                        let pUnion = parseWord "|"
                        let parseRhs = 
                            pUnion >>. pSpaces >>. parseAlgebraicType
                            |> many 1
                        return! parseUnit .>> pSpaces .>>. parseRhs
                    } <?> "Union" |>> fun res -> Union(fst res :: snd res)
                and parseMult = 
                    Parser {
                        let pIntersection = parseWord "&"
                        let parseRhs = 
                            pIntersection >>. pSpaces >>. parseAlgebraicType
                            |> many 1
                        return! parseUnit .>> pSpaces .>>. parseRhs
                    } <?> "Intersection" |>> fun res -> Intersection(fst res :: snd res)
                and parseExp = 
                    Parser {
                        let pExponent = parseWord "->"
                        let parseRhs = 
                            pExponent .>> pSpaces >>. parseAlgebraicType  
                            |> many 1
                        return! parseUnit.>> pSpaces .>>. parseRhs
                    } <?> "Exponent" |>> fun res -> Exponent(fst res :: snd res)
                and parseStruct = 
                    Parser {
                        let (pOpenCurly, pDoublePts ,pCloseCurly, pSemicolon) =  (parseWord "{", parseWord ":", parseWord "}", parseWord ";")
                        let parseField = parseIdentifier .>> pSpaces .>> pDoublePts .>> pSpaces .>>. parseType .>> pSpaces .>> pSemicolon
                        let parseFields = many 1 parseField
                        let pStructDef = pOpenCurly >>. pSpaces >>. parseFields .>> pSpaces .>> pCloseCurly
                        return! pStructDef
                    } <?> "Struct" |>> (Map.ofList >> Struct)
                parseStruct <|> parseSum <|> parseMult <|> parseExp <|> parseUnit 
            Parser {        
                let [typeDecl; consumeEq] = ["type"; "="] |> List.map parseWord
                let! result = typeDecl >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. parseAlgebraicType
                return result
            } <?> "Type Definition" |>> TypeDefinition
        and parseLibrary = 
            let mapper = fun ((a,b),c) -> (a,b,c)
            let parseMapping = 
                Parser {
                    let [|consumeTyper;  consumeEq|] = [|":"; "="|] |> Array.map parseWord
                    let pKey = parseIdentifier
                    let pValue = parseValue <|> parseFunction <|> (parseLet false) 
                    return! pKey .>> pSpaces .>> consumeTyper .>> pSpaces .>>. parseType .>> pSpaces .>> consumeEq .>> pSpaces .>>. pValue
                }
            Parser {
                return! (pSpaces >>. (expect ';') >>. pSpaces)
                            |> separateBy 1 parseMapping
                            |> betweenC ('{', '}')
            } <?> "Library" |>> ((List.map mapper) >> Library)
        and parseExpression = 
            Parser {
                return! [
                    parseTypeDef
                    parseLibrary
                    parseBrancher
                    parseInclude    
                    parseLet false
                    parseLibrary
                    parseCompound
                    parseFunction
                    parseBinary
                    parseOperation      
                    parseValue
                    parseUnary
                    parseIdentifier    
                ] |> choice 
            } <?> "Expression" 
        parseLet true

    let parse text = Parsec.Parser.run (fromStr text) parseExpr