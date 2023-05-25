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
        let rec parseExpression includeTypeAsValueParser = 
            Parser {
                return! [
                    parseLibrary includeTypeAsValueParser
                    parseMatch
                    parseBrancher 
                    parseInclude    
                    parseLet false
                    parseCompound
                    parseFunction
                    parseOperation      
                    parseValue includeTypeAsValueParser
                    parseBinary
                    parseUnary
                    parseIdentifier    
                ] |> choice 
            } <?> "Expression"
        and parseType = 
            let rec parseUnit= 
                Parser {
                    return! option (forName |> Seq.toList |> anyOf |> many 1) 
                } <?> "Atom" 
                    |>> function 
                        | Some(value) -> value |> toString |> (fun str -> str.Trim()) |> Atom
                        | None        -> Atom String.Empty
            and parseSum = 
                Parser {
                    let pUnion = parseWord "|"
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
        and parseValue includeTypeParser=
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
                        pSpaces >>. pColon >>. pSpaces >>. parseValue false
                        |> many 0
                    return! pOpenParen >>. (parseValue false).>>. parseLhs .>> pCloseParen 
                } <?> "Tuple" |>> fun (fst, tail) -> Value <| Tuple(fst::tail)
            // and parseConstructor = 
            //     Parser {
            //         let [| pMake |] = [| "new"|] 
            //                                     |> Array.map parseWord
            //         return! pMake >>. parseIdentifier .>> pSpaces .>>. parseValue
            //     } <?> "Tuple" |>> (Constructor >> Value)
            and parseRecord =  (parseLibrary false) <?> "Record" 
            and parseList = 
                Parser {
                    let pElems= (pSpaces >>.  (',' |> expect) >>. pSpaces)
                                    |> separateBy 0 (parseExpression false)
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
            and parseTypeDefinition = parseType |>> (TypeDefinition >> Value)
            //parseSimple <|> parseList <|> parseString <|> parseTuple <|> parseConstructor <|> parseRecord
            let core_parser = parseSimple <|> parseList <|> parseString <|> parseTuple <|> parseRecord 
            if not includeTypeParser then core_parser 
            else core_parser <|> parseTypeDefinition 
        and parseLet topLevel =
                let mapper = fun (((var, var_t),b),c) -> (var, var_t, b, c)
                Parser {
                    let [|consumeLet; consumeIn; consumeEnd; consumeTyper;  consumeEq|] = [|"let"; "in"; "end"; ":"; "="|] |> Array.map parseWord
                    return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeTyper .>> pSpaces .>>. parseType .>> pSpaces .>> consumeEq .>> pSpaces .>>. (parseExpression true)
                                      .>>  pSpaces .>> (if topLevel then consumeEnd else consumeIn) .>>  pSpaces   .>>. parseExpression false
                } <?> "Binder" |>> ( mapper >> Bind )
        and parseCompound =
                Parser {
                    let [|consumeWhere; consumeTyper; consumeEq; consumeAnd|] = [|"where"; ":"; "="; "and"|] |> Array.map parseWord
                    let parseExpression =   choice [    
                                                parseBrancher; parseFunction; parseBinary; parseOperation; parseValue false; parseUnary; parseIdentifier    
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
                let pCondition = choice [parseUnary; parseBinary; parseOperation; parseValue false; parseIdentifier]
                return! consumeIf   >>. pSpaces  >>. pCondition      .>> pSpaces 
                    .>> consumeThen .>> pSpaces .>>. (parseExpression false) .>> pSpaces 
                    .>> consumeElse .>> pSpaces .>>. (parseExpression false)
            } |>> ( mapper >> Branch )  <?> "Brancher" 
        and parseIdentifier = 
            '_'::printable
            |> Seq.toList 
            |> anyOf |> many 1 
            <?> "Identifier" |>> (toString >> Identifier)
        
        and parseFunction  = 
            Parser {
                let [pArrow ; consumeTyper] = ["=>"; ":"] |> List.map parseWord
                let pArg  = parseIdentifier .>> pSpaces .>> consumeTyper .>> pSpaces .>>. parseType
                let mParams =  (pSpaces >>. (','  |> expect) >>. pSpaces)
                                    |> separateBy 1  pArg
                                    |> betweenC ('(', ')')
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. (parseExpression false)
            } <?> "Function" |>> Function
        and parseUnary = 
            Parser {
                let uniOper = ['~';'-';'Y'] |> anyOf |> many 1 |>> Operation.toOp
                              <|> ( "rec" |> Seq.toList 
                                    |> allOf |>> fun _ -> YComb)
                let uniExpr =   (betweenC ('(', ')') parseUnary)  
                                <|> choice [ parseBinary; parseOperation; parseValue false; parseFunction; parseIdentifier] 
                return! uniOper .>> pSpaces .>>. uniExpr
            } <?> "Unary" |>>  Unary
        and parseOperation  = 
            let legacyParser = 
                Parser {
                    let pArgs=  (pSpaces >>. (',' |> expect) >>. pSpaces)
                                    |> separateBy 1 (parseExpression false)
                                    |> betweenC ('(', ')') 
                    let pOpr = parseIdentifier <|> (betweenC ('(', ')') (parseExpression false))
                    return! pOpr .>>. pArgs
                } 
            (legacyParser) <?> "Applicative" |>> Application
        and parseMatch = 
            Parser {
                let [|pMatch; pLine; pWith|] = [|"match"; "|"; "with"|] |> Array.map parseWord
                let parseFirstLine   = pMatch >>. pSpaces >>. parseIdentifier .>> pSpaces .>> pWith .>> pSpaces
                let parseTypePattern = pLine  >>. pSpaces >>. parseFunction  .>> pSpaces 
                let parseFallbackPat = 
                    let [| pUnderscore; pArrow |] = [|"_"; "=>"|] |> Array.map parseWord
                    pLine  >>. pSpaces >>. pUnderscore >>. pSpaces >>. pArrow  .>> pSpaces >>. (parseExpression false) .>> pSpaces
                return! parseFirstLine .>>. (many 1 parseTypePattern) .>>. (option parseFallbackPat)
            } <?> "Match" |>> fun ((a, b),c) -> Statement.Match(a, b, c)
        and parseBinary  = 
            Parser {
                let operand =   (betweenC ('(', ')') parseBinary)   
                                <|> choice [ parseUnary; parseOperation; parseValue false; parseIdentifier] 
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
                return! consumeInclude >>. pSpaces >>. parsefiles .>> pSpaces .>> consumeFor .>> pSpaces .>>. (parseExpression false)
            } <?> "Include" |>> Context
        and parseLibrary includeTypeAsValueParser = 
            let mapper = fun ((a,b),c) -> (a,b,c)
            let parseMapping = 
                Parser {
                    let [|consumeTyper;  consumeEq; consumeSemCol|] = [|":"; "="; ";"|] |> Array.map parseWord
                    let pKey = parseIdentifier
                    let pValue = (parseValue includeTypeAsValueParser) <|> parseFunction <|> (parseLet false) 
                    return! pKey .>> pSpaces .>> consumeTyper .>> pSpaces .>>. parseType .>> pSpaces .>> consumeEq .>> pSpaces .>>. pValue .>> pSpaces .>> consumeSemCol .>> pSpaces
                }
            Parser {
                let (pOpenCurly, pCloseCurly) =  (parseWord "{", parseWord "}")
                return! pOpenCurly >>. pSpaces >>. (many 1 parseMapping) .>> pSpaces .>> pCloseCurly;
            } <?> "Library" |>> ((List.map mapper) >> Record >> Value) 
        parseLet true

    let parse text = Parsec.Parser.run (fromStr text) parseExpr