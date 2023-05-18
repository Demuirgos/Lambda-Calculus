module Typechecker 

    open System
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
            let bodyType = TypeOf ctx body 
            match bodyType with
            | Ok(term_t) 
                when term_t = suggested_type || suggested_type = Atom String.Empty ->
                    let ctx = 
                        if suggested_type = Atom String.Empty
                        then ctx.Add(name, term_t) 
                        else ctx.Add(name, suggested_type)
                    TypeOf ctx cont
            | Ok(term_t) -> Error (sprintf "Type mismatch in bind : expected type %s but given type %s" (suggested_type.ToString()) (term_t.ToString())) 
            | error -> error 
        | Branch(_cond, _then, _else) -> 
            let condType = TypeOf ctx _cond
            let thenType = TypeOf ctx _then
            let elseType = TypeOf ctx _else 
            match condType, thenType, elseType with
            | Ok(Atom "bool"), Ok(then_t), Ok(else_t) when then_t = else_t -> Ok(then_t)
            | Ok(Atom "bool"), Ok(then_t), Ok(else_t) -> 
                Error (sprintf "Type mismatch in if-expr : expression branches types don't match %s and %s" (then_t.ToString()) (else_t.ToString())) 
            | Ok cond_t, _, _ -> Error (sprintf "Type mismatch in if-expr : Condition Type must be %s but given %s" (Atom("bool").ToString()) (cond_t.ToString())) 
            | Error err, _, _ | _, Error err, _ | _, _ , Error err -> Error err
        | Identifier(name)  -> 
            if(ctx.ContainsKey(name)) 
            then Ok ctx[name]
            else Error (sprintf "Undefined identifier %s" name)
        | Function(in_vs, out_v) ->
            let ctx = addRange in_vs ctx
            match TypeOf ctx  out_v with 
            | Ok(out_t) -> 
                let rec constructType res = 
                    function
                    | [] -> Error "Function expects at least one argument"
                    | [(_, arg_t)] -> Ok (Arrow(arg_t, res))
                    | (_, arg_t):: t ->  constructType (Arrow(arg_t, res)) t
                in_vs |> List.rev |> constructType out_t
            | error -> error
        | Application(func, args)  -> 
            let func_t = TypeOf ctx func 
            let args_t = args |> List.map (TypeOf ctx) 
            let rec checkValidity func args = 
                match func, args with
                | Ok t , [] -> Ok (t)
                | Ok (Arrow(in_t, out_t)), (Ok t):: ts 
                    when in_t = t -> checkValidity (Ok out_t) ts
                | Ok (Arrow(in_t, _)), (Ok t):: ts -> 
                    Error (sprintf "Type mismatch in application : expected type %s but given type %s" (in_t.ToString()) (t.ToString()))
                | _, (Error err):: _ | Error err, _ -> Error err
                | _ -> Error "Type mismatch in application"
            checkValidity func_t args_t
        | Compound(expr, binds) -> 
            let rec typesList binds res = 
                match binds with
                | [] -> Ok res
                | ((iden, id_t), body) :: t -> 
                    match TypeOf ctx body with
                    | Ok type_r when type_r = id_t || id_t = Atom "" -> typesList t ((iden, type_r)::res) 
                    | Ok type_r -> Error (sprintf "Type mismatch in compound : expected type %s but given type %s" (id_t.ToString()) (type_r.ToString()))
                    | Error err -> Error err
            let bindTypes = typesList binds []
            match bindTypes with
            | Ok binds_t -> 
                let ctx = addRange binds_t ctx
                TypeOf ctx expr
            | Error msg -> Error msg 
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
            | Ok (left_t), Ok (right_t) -> 
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
                | Custom symbol, loperand_t, roperand_t -> 
                    if(ctx.ContainsKey(symbol)) then 
                        let typeOfExpr = 
                            let typeOfBinaryOp = TypeOf ctx (Identifier symbol) 
                            match typeOfBinaryOp with
                            | Ok (Arrow(lhs_t, Arrow(rhs_t, out_t))) 
                                when lhs_t = loperand_t && rhs_t = roperand_t-> Ok out_t
                            | Ok (Arrow(lhs_t, Arrow(rhs_t, out_t)) as binop_t) 
                                -> Error (sprintf "Type mismatch in binary operation : expected type %s but given type %s" (binop_t.ToString()) (Arrow(loperand_t, Arrow(roperand_t, out_t)).ToString())) 
                            | error -> error 
                        typeOfExpr
                    else Error (sprintf "Undefined binary operation %s" symbol)
                | _ -> Error "Unsupported binary operation"
            | Error msg, _ | _, Error msg -> Error msg 
        | Context (stmts, program) -> TypeOf ctx program
        | TypeDefinition(Identifier(typeName), type_t, cont) -> TypeOf (Map.add typeName (Type.Custom type_t) ctx) cont


        // Statement * ((Statement * Type) * Statement) list