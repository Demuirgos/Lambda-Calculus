module Typechecker 

    open Parsec
    open Typedefinitions
    open System.IO

    let rec addRange l ctx: TypingContext = 
        match l with 
        | [] -> ctx
        | (Identifier(name), arg_t):: t -> addRange t (ctx.Add(name, arg_t))  
        | _ -> failwith "Invalid type definition"
    
    let rec TypeOf (ctx : TypingContext) term = 
        match term with 
        | Bind(Identifier(name), suggested_type, body, cont) -> 
            let ctx = ctx.Add(name, suggested_type)
            let bodyType = TypeOf ctx body 
            match bodyType with
            | Ok(term_t) 
                when term_t = suggested_type || suggested_type = Atom "" ->
                    if suggested_type = Atom "" 
                    then TypeOf (ctx.Add(name, term_t)) cont 
                    else TypeOf ctx cont
            | _ -> 
                Error ("Type mismatch") 
        | Branch(_cond, _then, _else) -> 
            let condType = TypeOf ctx _cond
            let thenType = TypeOf ctx _then
            let elseType = TypeOf ctx _else 
            match condType, thenType, elseType with
            | Ok(Atom "bool"), Ok(then_t), Ok(else_t) when then_t = else_t -> Ok(then_t)
            | _ -> Error ("Type mismatch")
        | Identifier(name)  -> 
            if(ctx.ContainsKey(name)) 
            then Ok ctx[name]
            else Error "Type error"
        | Function(in_vs, out_v) ->
            let ctx = addRange in_vs ctx
            match TypeOf ctx  out_v with 
            | Ok(out_t) -> 
                let rec constructType = 
                    function
                    | [] -> failwith "Type error"
                    | [(_, arg_t)] -> Arrow(arg_t, out_t)
                    | (_, arg_t):: t ->  Arrow(arg_t, constructType t)
                Ok <| constructType in_vs
            | _ -> Error "Type error"
        | Application(func, args)  -> 
            let func_t = TypeOf ctx func 
            let args_t = args |> List.map (TypeOf ctx) 
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
                    match TypeOf ctx body with
                    | Ok type_r when type_r = id_t -> typesList t ((iden, id_t)::res) 
                    | _ -> Error "Type error"
            let bindTypes = typesList binds []
            match bindTypes with
            | Ok binds_t -> 
                let ctx = addRange binds_t ctx
                TypeOf ctx expr
            | _ -> Error "Type error" 
        | Value lit -> 
            match lit with 
            | Variable _ -> Ok <| Atom "number"
            | String _   -> Ok <| Atom "word"
            | List _     -> Ok <| Atom "list"
            | Bool _     -> Ok <| Atom "bool"
            | _          -> Error "Type error"
        | Binary(left, op, right) -> 
            let left_t = TypeOf ctx left
            let right_t = TypeOf ctx right
            match left_t, right_t with
            | Ok (left_t), Ok (right_t) when left_t = right_t -> 
                match op, left_t, right_t with
                | Add , Atom "number" , Atom "number" -> Ok (Atom "number")
                | Subs, Atom "number" , Atom "number" -> Ok (Atom "number")
                | Mult, Atom "number" , Atom "number" -> Ok (Atom "number")
                | Div , Atom "number" , Atom "number" -> Ok (Atom "number")
                | Exp , Atom "number" , Atom "number" -> Ok (Atom "number")
                | Eq  , Atom "number" , Atom "number" -> Ok (Atom "bool")
                | Gt  , Atom "number" , Atom "number" -> Ok (Atom "bool")
                | Lt  , Atom "number" , Atom "number" -> Ok (Atom "bool")
                | And , Atom "bool"   , Atom "bool"   -> Ok (Atom "bool")
                | Or  , Atom "bool"   , Atom "bool"   -> Ok (Atom "bool")
                | Not , Atom "bool"   , Atom "bool"   -> Ok (Atom "bool")
                | Xor , Atom "bool"   , Atom "bool"   -> Ok (Atom "bool")
                | Cons, Atom _        , Atom "list"   -> Ok (Atom "list")
                | Custom symbol, lhs_t, rhs_t -> 
                    if(ctx.ContainsKey(symbol)) then 
                        let typeOfExpr = 
                            Function ([(left, left_t); (right, right_t)] , Identifier symbol) 
                            |> TypeOf ctx 
                        typeOfExpr
                    else Error "Type error" 
                | _ -> Error "Type error"
            | _ -> Error "Type error"


        // Statement * ((Statement * Type) * Statement) list