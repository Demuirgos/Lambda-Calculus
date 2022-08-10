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

    let rec typeof term (ctx : TypingContext) = 
        match term with 
        | Bind(Identifier(name), tipe, body, cont) -> 
            let ctx = ctx.Add(name, tipe)
            let bodyType = typeof body ctx
            let contType = typeof cont ctx
            match bodyType, contType with
            | Ok(term_t), Ok(cont_t) when term_t = tipe -> Ok(cont_t)
            | _ -> Error ("Type mismatch") 
        | Branch(_cond, _then, _else) -> 
            let condType = typeof _cond ctx
            let thenType = typeof _then ctx
            let elseType = typeof _else ctx
            match condType, thenType, elseType with
            | Ok(bool), Ok(then_t), Ok(else_t) when then_t = else_t -> Ok(then_t)
            | _ -> Error ("Type mismatch")
        | Identifier(name)  -> 
            if(ctx.ContainsKey(name)) 
            then Ok ctx[name]
            else Error "Type error"
        | Function([(in_p, in_t)], out_v) -> 
            match typeof out_v ctx with 
            | Ok(out_t) -> Ok <| Arrow(in_t, out_t)
            | _ -> Error "Type error"
        | Application(func, [arg])  -> 
            let func_t = typeof func ctx
            let arg_t = typeof arg ctx
            match func_t, arg_t with
            | Ok(Arrow(in_t, out_t)), Ok(arg_t) when arg_t = in_t -> Ok out_t
            | _ -> Error "Type error" 

        