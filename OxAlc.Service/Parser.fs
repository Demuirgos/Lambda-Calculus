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
        let rec parseType = 
            let rec parseUnit= 
                Parser {
                    return! option (forName |> Seq.toList |> anyOf |> many 1) 
                } <?> "Atom" 
                    |>> function 
                        | Some(value) -> value |> toString |> (fun str -> str.Trim()) |> Atom
                        | None        -> Atom String.Empty
            and parseSum = 
                Parser {
                    let (pUnion, pColon) = (parseWord "|", parseWord ":")
                    // let pUnit = parseUnit .>>. option (pColon >>. parseExp)
                    let parseRhs = 
                        pSpaces >>. pUnion >>. pSpaces >>. parseType
                        |> many 1
                    return! parseUnit .>>. parseRhs
                } <?> "Union" |>> fun (fst, tail) -> Union((fst :: tail)) 
            and parseMult = 
                Parser {
                    let pIntersection = parseWord "&"
                    let parseRhs = 
                        pSpaces >>. pIntersection >>. pSpaces >>. parseType
                        |> many 1
                    return! parseUnit .>>. parseRhs
                } <?> "Intersection" |>> fun (fst, tail) -> Intersection(fst :: tail)
            and parseExp = 
                Parser {
                    let pExponent = parseWord "->"
                    return! parseUnit .>> pSpaces .>> pExponent .>> pSpaces .>>. parseType
                } <?> "Exponent" |>> Exponent
            and parseStruct = 
                Parser {
                    let (pOpenCurly, pDoublePts ,pCloseCurly, pSemicolon) =  (parseWord "{", parseWord ":", parseWord "}", parseWord ";")
                    let parseField = parseIdentifier .>> pSpaces .>> pDoublePts .>> pSpaces .>>. parseType .>> pSpaces .>> pSemicolon .>> pSpaces
                    let parseFields = many 1 parseField
                    let pStructDef = pOpenCurly >>. pSpaces >>. parseFields .>> pSpaces .>> pCloseCurly
                    return! pStructDef
                } <?> "Struct" |>> (Map.ofList >> Struct)
            parseStruct <|> parseSum <|> parseMult <|> parseExp <|> parseUnit 
        and parseLet topLevel =
                let mapper = fun (((var, var_t),b),c) -> (var, var_t, b, c)
                Parser {
                    let [|consumeLet; consumeIn; consumeEnd; consumeTyper;  consumeEq|] = [|"let"; "in"; "end"; ":"; "="|] |> Array.map parseWord
                    return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeTyper .>> pSpaces .>>. parseType .>> pSpaces .>> consumeEq .>> pSpaces .>>. parseExpression
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
            and parseTuple = 
                Parser {
                    let [| pOpenParen ; pColon; pCloseParen |] = 
                        [| "("; "," ;")"|] |> Array.map parseWord
                    let parseLhs = 
                        pSpaces >>. pColon >>. pSpaces >>. parseValue
                        |> many 0
                    return! pOpenParen >>. parseValue .>>. parseLhs .>> pCloseParen 
                } <?> "Tuple" |>> fun (fst, tail) -> Value <| Tuple(fst::tail)
            // and parseConstructor = 
            //     Parser {
            //         let [| pMake |] = [| "new"|] 
            //                                     |> Array.map parseWord
            //         return! pMake >>. parseIdentifier .>> pSpaces .>>. parseValue
            //     } <?> "Tuple" |>> (Constructor >> Value)
            and parseRecord =  parseLibrary <?> "Record" 
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
            //parseSimple <|> parseList <|> parseString <|> parseTuple <|> parseConstructor <|> parseRecord
            parseSimple <|> parseList <|> parseString <|> parseTuple <|> parseRecord
        and parseFunction  = 
            Parser {
                let [pArrow ; consumeTyper] = ["=>"; ":"] |> List.map parseWord
                let pArg  = parseIdentifier .>> pSpaces .>> consumeTyper .>> pSpaces .>>. parseType
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
            Parser {        
                let [typeDecl; consumeEq; parseIn] = ["type"; "="; "in"] |> List.map parseWord
                let! result = typeDecl >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. parseType .>> pSpaces .>> (option parseIn) .>> pSpaces .>>. parseExpression
                return result
            } <?> "Type Definition" |>> fun ((iden, typeValue), cont) -> TypeDefinition(iden, typeValue, cont)
        and parseLibrary = 
            let mapper = fun ((a,b),c) -> (a,b,c)
            let parseMapping = 
                Parser {
                    let [|consumeTyper;  consumeEq; consumeSemCol|] = [|":"; "="; ";"|] |> Array.map parseWord
                    let pKey = parseIdentifier
                    let pValue = parseValue <|> parseFunction <|> (parseLet false) 
                    return! pKey .>> pSpaces .>> consumeTyper .>> pSpaces .>>. parseType .>> pSpaces .>> consumeEq .>> pSpaces .>>. pValue .>> pSpaces .>> consumeSemCol .>> pSpaces
                }
            Parser {
                let (pOpenCurly, pCloseCurly) =  (parseWord "{", parseWord "}")
                return! pOpenCurly >>. pSpaces >>. (many 1 parseMapping) .>> pSpaces .>> pCloseCurly;
            } <?> "Library" |>> ((List.map mapper) >> Record >> Value)
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