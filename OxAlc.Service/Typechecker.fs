module Typechecker 

    open Parsec
    open Typedefinitions
    open System.IO

    let parseType = 
        let rec parseAtom = 
            Parser {
                return! ['a' .. 'z' ] |> Seq.toList |> anyOf |> many 1 
            } <?> "Atom" |>> ( toString >> Atom )
        and parseArrow = 
            Parser {
                let operand =   (betweenC ('(',')') parseArrow) <|> parseAtom 
                let binOper =  parserWord "->"
                return! operand .>>  pSpaces .>> binOper .>> pSpaces .>>. operand
            } <?> "Arrow" |>> Arrow
        parseArrow <|> parseAtom

    let rec addRange l ctx: TypingContext = 
        match l with 
        | [] -> ctx
        | (Identifier(name), arg_t):: t -> addRange t (ctx.Add(name, arg_t))  
    
    let rec typeof (ctx : TypingContext) term = 
        match term with 
        | Bind(Identifier(name), tipe, body, cont) -> 
            let ctx = ctx.Add(name, tipe)
            let bodyType = typeof ctx body 
            let contType = typeof ctx cont
            match bodyType, contType with
            | Ok(term_t), Ok(cont_t) when term_t = tipe -> Ok(cont_t)
            | _ -> Error ("Type mismatch") 
        | Branch(_cond, _then, _else) -> 
            let condType = typeof ctx _cond
            let thenType = typeof ctx _then
            let elseType = typeof ctx _else 
            match condType, thenType, elseType with
            | Ok(bool), Ok(then_t), Ok(else_t) when then_t = else_t -> Ok(then_t)
            | _ -> Error ("Type mismatch")
        | Identifier(name)  -> 
            if(ctx.ContainsKey(name)) 
            then Ok ctx[name]
            else Error "Type error"
        | Function(in_vs, out_v) ->
            let ctx = addRange in_vs ctx
            match typeof ctx  out_v with 
            | Ok(out_t) -> 
                let rec constructType = 
                    function
                    | [] -> failwith "Type error"
                    | [(_, arg_t)] -> Arrow(arg_t, out_t)
                    | (_, arg_t):: t ->  Arrow(arg_t, constructType t)
                Ok <| constructType in_vs
            | _ -> Error "Type error"
        | Application(func, args)  -> 
            let func_t = typeof ctx func 
            let args_t = args |> List.map (typeof ctx) 
            let rec checkValidity func args = 
                match func, args with
                | Ok t , [] -> Ok (t)
                | Ok (Arrow(in_t, out_t)), (Ok t):: ts 
                    when in_t = t -> checkValidity (Ok out_t) ts
                | _, (Error _):: _ | Error _, _ | _ -> Error "Type error"
            checkValidity func_t args_t
        | Compound(expr, binds) -> 
            let rec typesList binds res = 
                match binds with
                | [] -> Ok res
                | ((iden, id_t), body) :: t -> 
                    match typeof ctx body with
                    | Ok type_r when type_r = id_t -> typesList t ((iden, id_t)::res) 
                    | _ -> Error "Type error"
            let bindTypes = typesList binds []
            match bindTypes with
            | Ok binds_t -> 
                let ctx = addRange binds_t ctx
                typeof ctx expr
            | _ -> Error "Type error" 


        // Statement * ((Statement * Type) * Statement) list