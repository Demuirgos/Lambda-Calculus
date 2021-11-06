module Abstractor 
    open Interpreter
    open Parsec
    type Thunk = Interpreter.Expression 
    type Statement = 
        | Iden    of string 
        | Bind    of Statement * Statement 
        | List    of Statement list 
        | Term    of Statement list * Statement 
        | BiOp    of Statement * Statement * Statement 
        | Prog    of Statement list * Statement     
        | Oper    of Statement * Statement list      
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
                let pStatements = many 0 parseLet
                let returnVal = parseBinary <|> parseOperation <|> parseIden <|> parseList
                let! app = pStatements .>>. returnVal
                return app 
            } <?> "Applicative" |>> Prog
        and parseIden = 
            Parser {
                let! pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                return pVar
            } <?> "Identifier" |>> (toString >> Iden)
        and parseList = 
            Parser {
                let pElems p = between (expect '[') p (expect '}') 
                let elem = parseIden <|> parseList <|> parseTerm <|> parseOperation
                let elements =  separate1By (pElems elem) (expect ',') 
                return! elements
            } <?> "List" |>> List
        and parseTerm  = 
            Parser {
                let pArrow = "=>" |> Seq.toList |> allOf
                let pArgs = many 0 (parseIden .>> expect ',') .>> pSpaces .>>. parseIden
                let mParams = expect '(' >>. pSpaces >>. pArgs .>> pSpaces .>> expect ')'
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Lambda" |>> (fun ((args,arg),rhs) -> (args@[arg],rhs) |> Term)
        and parseBinary  = 
            Parser {
                let operand = parseIden <|> parseOperation
                let binOper = ['+';'-';'/';'*'] |> anyOf |>> (string >> Iden)
                return! operand .>>  pSpaces .>>. binOper .>> pSpaces .>>. operand
            } <?> "Binary Term" |>> (fun ((lhs,op),rhs) -> (lhs,op,rhs) |> BiOp)
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
                        parseTerm
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