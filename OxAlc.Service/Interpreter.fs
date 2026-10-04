module Interpreter
    open Parsec
    open FSharp.Core
    open System
    #nowarn "40"

    type Expression = 
        | Atom of string
        | Applicative of Expression * Expression
        | Lambda of Expression * Expression 
    and Envirement = list<string * Expression>

    
    let parseExpr = 
        let rec parseAtom =
            Parser {
                let! atom = [yield! ['a'..'z'] ;'_'] |> Seq.toList |> anyOf |> many 1 
                return atom
            } <?> "Atom" |>> (toString >> Atom)
        and parseApp = 
            Parser {
                let pParens p = between (expect '(') p (expect ')')
                let! app = pParens ( parseExpression .>> pSpaces .>>. parseExpression )
                return app 
            } <?> "Applicative" |>> Applicative
        and parseLambda = 
            Parser {
                let pLmbda = anyOf [ '\\'; 'λ']; 
                let pDot = expect '.'
                let! lmbda = pLmbda >>. parseAtom .>> pDot .>>. parseExpression 
                return lmbda
            } <?> "Lambda" |>> fun (param,body) -> Lambda (param ,body)
        and parseExpression  = 
            Parser {
                let! expr = 
                    choice [    
                        parseAtom     
                        parseApp        
                        parseLambda 
                    ] 
                return expr
            } <?> "Expression" 
        parseExpression
    
    let evalExpr= 
        let convert input =
            let mutable counter = 0
            let rec loop expr dict = 
                match expr with 
                | Atom id -> 
                    match Map.tryFind id dict with 
                    | Some v -> Atom v
                    | None -> expr
                | Lambda (Atom(id), body) -> 
                    let newId = sprintf "%s_%d" id counter
                    counter <- counter + 1
                    Lambda(Atom(newId), loop body (Map.add id newId dict))
                | Applicative(lhs, rhs) ->
                    Applicative(loop lhs dict, loop rhs dict)
            loop input Map.empty 

        let rec substitute expr arg value = 
            match expr with 
            | Atom id when id = arg -> value
            | Lambda (Atom id, body) when id <> arg -> 
                Lambda(Atom id, substitute body arg value)
            | Applicative(lhs, rhs) -> 
                Applicative(substitute lhs arg value, substitute rhs arg value)
            | _ -> expr

        let rec isNormalForm expr = 
            match expr with 
            | Atom _ -> true
            | Lambda (_, body) -> isNormalForm body
            | Applicative(lhs, rhs) ->
                match lhs with 
                | Lambda _ -> false
                | _ -> isNormalForm lhs && isNormalForm rhs

        let rec reduce expr =
            if isNormalForm expr then expr
            else 
                match  expr with 
                | Atom _ -> expr
                | Lambda (arg, body) ->
                    Lambda (arg, reduce body) 
                | Applicative(lhs, rhs) ->
                    match lhs with 
                    | Lambda (Atom(id), body) ->
                        reduce (substitute body id rhs)
                    | _ ->
                        reduce (Applicative (reduce lhs, reduce rhs))  

        convert >> reduce

    let toString expr = 
        let rec toString' term = 
            match term with 
            | Lambda(arg,body) -> 
                match arg with 
                | Atom(name) -> sprintf "λ%s.%s" name (toString' body)
                | _ -> failwith "invalid Lambda parameter"
            | Atom(v) -> sprintf "%s" v
            | Applicative(lhs,rhs) -> sprintf "(%s %s)" (toString' lhs) (toString' rhs)
        toString' expr
    let parse txt = (fromStr txt, parseExpr .>> eof) ||> run 
    let interpret (code: Expression) = code |> evalExpr
