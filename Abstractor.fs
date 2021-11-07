module Abstractor 
    open Interpreter
    open Parsec
    type Thunk = Interpreter.Expression 
    type Statement = 
        | Identifier        of string 
        | Bind              of Statement * Statement * Statement 
        | Lambda            of Statement list * Statement 
        | Application       of Statement * Statement list
    and BinaryOp   = Add | Subs | Div | Mult      
    #nowarn "40"

    // let x = 5 in 6                      => Bind(x,5)
                                        // => (\x.6 5)
    // let f = (y,z) -> z * y              => Bind(f,lambda([y;z], operation(mult,args(z,y))))
                                        // => \f.\y.\z.(z*y)
    // let a = f x                         => Bind(a,application(f,x)) 
                                        // => \a.(f x)
    // let a = [e;f;g;h]                   => Bind(a,cons(e,cons(f,cons(f,cons(g,cons(h,nil))))))

    // let z =                             => Bind(z,Programram([Bind(a,5);Bind(b,6)]m,binOp(a,add,b)))
        // let a = 5
        // let b = 6
        // a + b
    let parseBiOp = function
        | Identifier("+") -> Add 
        | Identifier("-") -> Subs
        | Identifier("*") -> Mult
        | Identifier("/") -> Div
        | _   -> failwith "Not a binary Operation"
    let parseExpr = 
        let rec parseLet =
            Parser {
                let consumeLet = "let" |> Seq.toList |> allOf
                let consumeIn  = "in" |> Seq.toList |> allOf
                let consumeEq  = expect '='
                let binds = parseExpression 
                return! consumeLet >>. pSpaces >>. parseIdentifier .>> pSpaces .>> consumeEq .>> pSpaces .>>. binds
                                  .>>  pSpaces .>> consumeIn  .>> pSpaces .>>. binds
            } <?> "Binder" |>> (fun ((a,b),c) -> (a,b,c) |> Bind)
        and parseIdentifier = 
            Parser {
                return! ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            } <?> "Identifier" |>> (toString >> Identifier)
        and parseLambda  = 
            Parser {
                let pArrow = "=>" |> Seq.toList |> allOf
                let pArgs = many 1 parseIdentifier 
                let mParams =  ','  |> expect >>. pSpaces
                                    |> separate1By pArgs
                                    |> betweenC ('(',')') 
                return! mParams .>> pSpaces .>> pArrow .>> pSpaces .>>. parseExpression
            } <?> "Lambda" |>> (fun (args, elem2) -> (List.concat args, elem2) |> Lambda)
        and parseOperation  = 
            Parser {
                let pArgs=  ','  |> expect >>. pSpaces
                                    |> separate1By parseExpression
                                    |> betweenC ('(',')')
                return! parseIdentifier .>>. pArgs
            } <?> "Applicative" |>> Application
        and parseExpression = 
            Parser {
                let! expr = 
                    choice [    
                        parseLet
                        parseOperation      
                        parseLambda
                        parseIdentifier    
                    ] 
                return expr
            } <?> "Expression" 
        parseLet
    let transpile input = 
        let Result = (fromStr input, parseExpr) 
                     ||> run
        match Result with
        | Success (program,_) -> 
            let rec emitLambda= function
                | Bind(name, expr, value) ->
                    sprintf "(\\%s.%s %s)" (emitLambda name) (emitLambda value) (emitLambda  expr)
                | Lambda(h::t, expr) -> 
                    sprintf "(\\%s.%s)" ((t) |> List.fold (fun acc e -> sprintf "%s %s" acc (emitLambda e)) (emitLambda h)) (emitLambda expr)
                | Application(expr, args) ->
                    sprintf "(%s%s)" (emitLambda expr) (args |> List.fold (fun acc e -> sprintf "%s %s" acc (emitLambda e)) "") 
                | Identifier(name) -> name
            emitLambda program
        | Failure _ -> toResult Result