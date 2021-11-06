module Abstractor 
    open Interpreter
    open Parsec
    type Thunk = Interpreter.Expression 
    type Statement = 
        | Iden    of string 
        | Bind    of Statement * Statement 
        | List    of Statement list 
        | Lambda    of Statement list * Statement 
        | BiOp    of Statement * BinaryOp * Statement 
        | Prog    of Statement list * Statement     
        | Oper    of Statement * Statement list
    and BinaryOp   = Add | Subs | Div | Mult      
    and Envirement = list<Expression * Thunk>
    #nowarn "40"

    // let x = 5                           => Bind(x,5)
                                        // => \x.5
    // let f = (y,z) -> z * y              => Bind(f,lambda([y;z], operation(mult,args(z,y))))
                                        // => \f.\y.\z.(z*y)
    // let a = f x                         => Bind(a,application(f,x)) 
                                        // => \a.(f x)
    // let a = [e;f;g;h]                   => Bind(a,cons(e,cons(f,cons(f,cons(g,cons(h,nil))))))

    // let z =                             => Bind(z,Program([Bind(a,5);Bind(b,6)]m,binOp(a,add,b)))
        // let a = 5
        // let b = 6
        // a + b
    let parseBiOp = function
        | Iden("+") -> Add 
        | Iden("-") -> Subs
        | Iden("*") -> Mult
        | Iden("/") -> Div
        | _   -> failwith "Not a binary Operation"
    let parseExpr = 
        let rec parseLet =
            Parser {
                let consumeLet = "let" |> Seq.toList |> allOf
                let consumeEq = expect '='
                let binds = parseExpression 
                let letparser = consumeLet >>. pSpaces >>. parseIden .>> pSpaces .>> consumeEq .>> pSpaces .>>. binds
                return! letparser
            } <?> "Let" |>> Bind
        and parseProgram = 
            Parser {
                let pStatements = many 0 (parseLet .>> anyOf ['\n'; '\r'])
                let returnVal = parseBinary <|> parseOperation <|> parseList <|> parseIden 
                let! app = pStatements .>>. returnVal
                return app 
            } <?> "Applicative" |>> Prog
        and parseIden = 
            Parser {
                return! ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            } <?> "Identifier" |>> (toString >> Iden)
        and parseList = 
            Parser {
                let pElem = parseBinary <|> parseIden <|> parseList <|> parseLambda <|> parseOperation
                let pList =  ';'|> expect >>. pSpaces 
                                |> separate1By pElem
                                |> betweenC ('[',']') 
                return! pList
            } <?> "List" |>> List
        and parseLambda  = 
            Parser {
                let pArrow = "=>" |> Seq.toList |> allOf
                let pArgs = many 1 parseIden 
                let mParams =  ','  |> expect >>. pSpaces
                                    |> separate1By pArgs
                                    |> betweenC ('(',')') 
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Lambda" |>> (fun (args, elem2) -> (List.concat args, elem2) |> Lambda)
        and parseBinary  = 
            Parser {
                let operand = parseIden <|> parseOperation
                let binOper = ['+';'-';'/';'*'] |> anyOf |>> (string  >> Iden)
                return! operand .>>  pSpaces .>>. binOper .>> pSpaces .>>. operand
            } <?> "Binary Operation" |>> (fun ((lhs,op),rhs) -> (lhs,parseBiOp op,rhs) |> BiOp)
        and parseOperation  = 
            Parser {
                let pArgs = pSpaces >>. parseIden |> many 1
                return! parseIden .>>. pArgs
            } <?> "Applicative Term" |>> Oper
        and parseExpression = 
            Parser {
                let! expr = 
                    choice [    
                        parseProgram
                        parseBinary     
                        parseOperation      
                        parseLambda
                        parseList             
                        parseIden    
                    ] 
                return expr
            } <?> "Expression" 
        parseLet
    let Transpile input = 
        (fromStr input, parseExpr) 
            ||> run 
             |> toResult