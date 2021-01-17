module Syntax 
    open Interpreter
    open Parsec
    type Thunk = Interpreter.Expression 
    type Statement = 
        | Iden    of Statement //*
        | Bind    of Statement * Statement list //*
        | List    of Statement list //*
        | Term    of Statement list * Statement //*
        | BiOp    of Statement * Statement * Statement //*
        | Prog    of Statement list * Statement     //*
        | Oper    of Statement * Statement list      
    and Envirement = list<Expression * Thunk>
    
    // let x = 5                           => let(x,5)
                                        // => \x.5
    // let f = (y,z) -> z * y              => let(f,lambda([y;z], operation(mult,args(z,y))))
                                        // => \f.\y.\z.(z*y)
    // let a = f x                         => let(a,application(f,x)) 
                                        // => \a.(f x)
    // let a = [e;f;g;h]                   => let(a,cons(e,cons(f,cons(f,cons(g,cons(h,nil))))))

    // let z =                             => let(z,Program([let(a,5);let(b,6)]m,binOp(a,add,b)))
        // let a = 5
        // let b = 6
        // a + b

    
    let parseExpr = 
        let rec parseLet =
            Parser {
                let consumeLet = "let" |> Seq.toList |> allOf
                let id = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                let consumeEq = expect '='
                let binds = parseProgram 
                let letparser = consumeLet >>. pSpaces >>. id .>> pSpaces .>> consumeEq .>>. binds
                return! letparser
            } <?> "Let" |>> Bind
        and parseProgram = 
            Parser {
                let pStatements = many 0 parseLet
                let returnVal = parseIden <|> parseOperation <|> parseBinary <|> parseList
                let! app = pStatements .>>. returnVal
                return app 
            } <?> "Applicative" |>> Prog
        and parseIden = 
            Parser {
                let pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
                return! pVar
            } <?> "Identifier" |>> Iden
        and parseList = 
            Parser {
                let pElems p = between (expect '[') p (expect '}') 
                let elem = parseIden <|> parseList <|> parseTerm <|> parseOperation
                let elements = pElems elem
                return! elements
            } <?> "List" |>> List
        and parseTerm  = 
            Parser {
                return! parseIden .>> pSpaces .>>. parseIden
            } <?> "Lambda" |> Term
        and parseBinary  = 
            Parser {
                let binOper = ['+','-','/','*'] |> anyOf
                return! parseIden .>>  pSpaces .>>. binOper .>> pSpaces .>>. parseIden
            } <?> "Binary Term" |> BiOp
        and parseOperation  = 
            Parser {
                let pArgs = pSpaces >>. parseIden |> many 0   
                return! parseIden .>>. pArgs
            } <?> "Applicative Term" |> Oper
        and parseExpression = 
            Parser {
                let! expr = 
                    choice [    
                        parseLet        ;  
                        parseOperation  ;    
                        parseBinary     ;
                        parseList       ;      
                        parseIden    
                    ] 
                return expr
            } <?> "Expression" 
        parseExpression
    