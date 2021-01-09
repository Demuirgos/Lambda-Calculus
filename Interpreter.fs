module Interpreter
    type Expression = 
        | Atom of string
        | Applicative of Expression * Expression
        | Lambda of string * Expression 
    and Envirement = string * Expression
<<<<<<< Updated upstream
        
    let rec parseAtom =
        Parser {
            let pword =['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            return! pword
        } <?> "Atom" |>> (toString >> Atom)
    and parseApp = 
        Parser {
            let pParens p = between (expect '(') p (expect ')')
            let pSpaces = many 1 (expect ' ')
            let pApp = pParens ( parseExpression .>> pSpaces .>>. parseExpression )
            return! pApp
        } <?> "Applicative" |>> Applicative
    and parseLambda = 
        Parser {
            let pLmbda = expect 'λ'
            let pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            let pDot = expect '.'
            let pLmbda = pLmbda >>. pVar .>> pDot .>>. parseExpression 
            return! pLmbda
        } <?> "Lambda" |>> fun (var,body) -> Lambda (toString var,body)
    and parseExpression  = 
        choice [    
            parseAtom;  
            parseApp;   
            parseLambda;    
        ] <?> "Expression"  
    let parse expr = 
        match run expr parseExpression  with 
        | Success (v)-> sprintf "%A" v
        | Failure (_) as f -> toResult f 
=======
<<<<<<< Updated upstream

    let Parse (input:char list) = 
        0
=======
        
    let rec parseAtom =
        Parser {
            printfn "here in parse atom again"
            let! atom = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            return atom
        } <?> "Atom" |>> (toString >> Atom)
    and parseApp = 
        Parser {
            printfn "here in parse applicative again"
            let pParens p = between (expect '(') p (expect ')')
            let pSpaces = many 1 (expect ' ')
            let! app = pParens ( parseExpression .>> pSpaces .>>. parseExpression )
            return app 
        } <?> "Applicative" |>> Applicative
    and parseLambda = 
        Parser {
            printfn "here in parse lambda again"
            let pLmbda = expect 'λ'
            let pVar = ['a'..'z'] |> Seq.toList |> anyOf |> many 1 
            let pDot = expect '.'
            let! lambda = pLmbda >>. pVar .>> pDot .>>. parseExpression 
            return lambda
        } <?> "Lambda" |>> fun (var,body) -> Lambda (toString var,body)
    and parseExpression  = 
        Parser {
            printfn "here in parse expr again"
            let! expr = 
                choice [    
                    parseAtom   ;  
                    parseApp    ;    
                    parseLambda    
                ] 
            return expr
        } <?> "Expression" 
         
    let parse expr = 
        match run expr parseExpression  with 
        | Success (v)-> sprintf "%A" v
        | Failure (_) as f -> toResult f 
>>>>>>> Stashed changes
>>>>>>> Stashed changes
