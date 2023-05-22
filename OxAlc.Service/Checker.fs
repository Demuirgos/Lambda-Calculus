module Typechecker 

    open System
    open Parsec
    open Typedefinitions
    open System.IO

    
    let addSymbol name type_n ctx = {
        ctx with 
            Symbols = Map.add name type_n ctx.Symbols  
    }

    let addType name type_n ctx = {
        ctx with 
            Types = Map.add name type_n ctx.Types  
    }

    let rec addRange l ctx: TypingContext = 
        match l with 
        | [] -> ctx
        | (Identifier(name), arg_t):: t -> addRange t (addSymbol name arg_t ctx)
        | _ -> failwith "Invalid type definition"
    
    // check suggested types if they alias type infered
    let rec TypeOf term (ctx : TypingContext) : (Result<Type, string> * TypingContext) = 
        match term with 
        | Bind(Identifier(name), suggested_type, body, cont) -> 
            let (bodyType, ctx) = TypeOf body ctx  
            match suggested_type, bodyType with
            | _ as desiredTyper, Ok(term_t) 
                when term_t = desiredTyper || desiredTyper = Atom String.Empty ->
                    let ctx = 
                        match term_t, body with 
                        | Atom "type", Value(TypeDefinition(type_def)) ->
                            addType name type_def ctx
                        | _ -> 
                            addSymbol name (if suggested_type = Atom String.Empty
                                then term_t 
                                else suggested_type)  ctx
                    TypeOf cont ctx 
            | Atom(identifier), Ok(term_t) when Map.containsKey identifier ctx.Types && term_t = (Map.find identifier ctx.Types) ->
                TypeOf cont (addSymbol name term_t ctx)  
            | _, Ok(term_t) -> Error (sprintf "Type mismatch in bind : expected type %s but given type %s" (suggested_type.ToString()) (term_t.ToString())), ctx
            | _, error -> error, ctx
        | Branch(_cond, _then, _else) -> 
            let (condType, ctx) = TypeOf _cond ctx 
            let (thenType, ctx) = TypeOf _then ctx 
            let (elseType, ctx) = TypeOf _else ctx  
            match condType, thenType, elseType with
            | Ok(Atom "bool"), Ok(then_t), Ok(else_t) when then_t = else_t -> Ok(then_t), ctx
            | Ok(Atom "bool"), Ok(then_t), Ok(else_t) -> 
                Error (sprintf "Type mismatch in if-expr : expression branches types don't match %s and %s" (then_t.ToString()) (else_t.ToString())), ctx
            | Ok cond_t, _, _ -> Error (sprintf "Type mismatch in if-expr : Condition Type must be %s but given %s" (Atom("bool").ToString()) (cond_t.ToString())), ctx
            | Error err, _, _ | _, Error err, _ | _, _ , Error err -> Error err, ctx
        | Identifier(name)  -> 
            if(ctx.Symbols.ContainsKey(name)) 
            then Ok ctx.Symbols[name], ctx
            else Error (sprintf "Undefined identifier %s" name), ctx
        | Function(in_vs, out_v) ->
            let ctx = addRange in_vs ctx
            match TypeOf out_v ctx with 
            | Ok(out_t), ctx -> 
                let rec constructType res = 
                    function
                    | [] -> Error "Function expects at least one argument"
                    | [(_, arg_t)] -> Ok (Exponent(arg_t, res))
                    | (_, arg_t):: t ->  constructType (Exponent(arg_t, res)) t
                (in_vs |> List.rev |> constructType out_t), ctx
            | error -> error
        | Application(func, args)  -> 
            let func_t = 
                match TypeOf func ctx with 
                | Ok (Atom(alias)), ctx -> 
                    match Map.tryFind alias ctx.Types with 
                    | Some type_instance -> Ok (type_instance)
                    | None -> Error(sprintf "type alias %s not found" alias)
                | Ok _ as type_name , _ -> type_name
                | _ as error, _ -> error 
            let args_t = args |> List.map ((fun arg -> TypeOf arg ctx) >> fst) 
            let rec checkValidity func args = 
                match func, args with
                | Ok t , [] -> Ok (t)
                | Ok (Exponent(in_t, out_t)), (Ok t):: ts 
                    when in_t = t -> checkValidity (Ok out_t) ts
                | Ok (Exponent(in_t, _)), (Ok t):: ts -> 
                    Error (sprintf "Type mismatch in application : expected type %s but given type %s" (in_t.ToString()) (t.ToString()))
                | _, (Error err):: _ | Error err, _ -> Error err
                | _ -> Error "Type mismatch in application"
            checkValidity func_t args_t, ctx
        | Compound(expr, binds) -> 
            let rec typesList binds res = 
                match binds with
                | [] -> Ok res
                | ((iden, id_t), body) :: t -> 
                    let (typeing_result, ctx) = TypeOf body ctx 
                    match typeing_result with
                    | Ok type_r when type_r = id_t || id_t = Atom "" -> typesList t ((iden, type_r)::res) 
                    | Ok type_r -> Error (sprintf "Type mismatch in compound : expected type %s but given type %s" (id_t.ToString()) (type_r.ToString()))
                    | Error err -> Error err
            let bindTypes = typesList binds []
            match bindTypes with
            | Ok binds_t -> 
                let ctx = addRange binds_t ctx
                TypeOf expr ctx
            | Error msg -> Error msg, ctx
        | Value lit -> 
            match lit with 
            | TypeDefinition _ -> Ok <| Atom "type", ctx
            | Variable _ -> Ok <| Atom "number", ctx
            | String _   -> Ok <| Atom "word", ctx
            | List _     -> Ok <| Atom "list", ctx
            | Bool _     -> Ok <| Atom "bool", ctx
            | Tuple stmt -> 
                let rec typeElements items types ctx= 
                    match items, types with 
                    | _, (Error _ as e)::_ -> e
                    | [], _ -> 
                        let unpealTypes typeResults = 
                            typeResults |> 
                                List.map (
                                    function 
                                    | Ok type_n -> type_n
                                    | _ -> failwith "Unreachable code")
                        Ok (Intersection (unpealTypes types))
                    | h::t, _ -> 
                        let (type_result, ctx) = TypeOf h ctx
                        typeElements t (type_result::types) ctx
                (typeElements (List.rev stmt) [] ctx), ctx
            | Record rcrd -> 
                let filterStructType = 
                    let types_in_ctx = Map.values ctx.Types 
                    Seq.toList <| Seq.filter (fun type_entry -> 
                        match type_entry with 
                        | Struct typedef -> 
                            rcrd
                            |>  List.forall (
                                    fun (name, type_n, value) -> 
                                        let value_infered_type = TypeOf value ctx
                                        match type_n with 
                                        | Atom "" -> 
                                            match value_infered_type with 
                                            | Ok field_type, _ -> (Map.containsKey name typedef) && (field_type = (Map.find name typedef))
                                            | _ -> false
                                        | Atom suggestedName as type_sn-> 
                                            let actualType = if Map.containsKey suggestedName ctx.Types then Map.find suggestedName ctx.Types else type_sn
                                            match value_infered_type with 
                                            | Ok field_type, _ when field_type = actualType  -> (Map.containsKey name typedef) && (type_n = (Map.find name typedef))
                                            | _ -> false
                                        | _ when (Map.containsKey name typedef) 
                                            && (type_n = (Map.find name typedef)) ->  
                                                match value_infered_type with
                                                | Ok field_type, _ -> field_type = type_n
                                                | _ -> false
                                ) 
                        | _ -> false
                    ) types_in_ctx
                let rec narrowType candidates = 
                    match candidates with 
                    | Struct(fields) as type_result::rest -> 
                        if Map.count fields = List.length rcrd then 
                            Ok(type_result)
                        else narrowType rest
                    | _ -> Error "Type error"

                narrowType filterStructType, ctx
            | _          -> Error "Type error", ctx
        | Binary(left, op, right) -> 
            let (left_t, ctx)  = TypeOf left ctx 
            let (right_t, ctx) = TypeOf right ctx 

            let rec handle op lhs_t rhs_t = 
                match op, lhs_t, rhs_t with
                | Add , Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "number")
                | Subs, Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "number")
                | Mult, Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "number")
                | Div , Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "number")
                | Exp , Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "number")
                | Eq  , Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "bool")
                | Gt  , Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "bool")
                | Lt  , Ok (Atom "number") , Ok(Atom "number") -> Ok (Atom "bool")
                | And , Ok (Atom "bool"  ) , Ok(Atom "bool"  ) -> Ok (Atom "bool")
                | Or  , Ok (Atom "bool"  ) , Ok(Atom "bool"  ) -> Ok (Atom "bool")
                | Not , Ok (Atom "bool"  ) , Ok(Atom "bool"  ) -> Ok (Atom "bool")
                | Xor , Ok (Atom "bool"  ) , Ok(Atom "bool"  ) -> Ok (Atom "bool")
                | Cons, Ok (Atom _       ) , Ok(Atom "list"  ) -> Ok (Atom "list")
                | Dot , Ok (Atom type_name) , _ -> 
                    handle Dot (match Map.tryFind type_name ctx.Types with | Some(t) -> Ok(t) | _ -> Error("Missing type")) right_t
                | Dot , Ok (Struct fields) , _ -> Ok (Map.find right fields)
                | Dot , Ok (Intersection items) , _ -> 
                    let item_index  = 
                        match right with 
                        | Value(Variable(index)) -> index 
                    Ok (items[item_index])
                | Custom symbol, Ok(loperand_t), Ok(roperand_t) -> 
                    if(ctx.Symbols.ContainsKey(symbol)) then 
                        let typeOfExpr = 
                            let typeOfBinaryOp = TypeOf (Identifier symbol) ctx
                            match typeOfBinaryOp with
                            | Ok (Exponent(lhs_t, Exponent(rhs_t, out_t))), _ 
                                when lhs_t = loperand_t && rhs_t = roperand_t-> Ok out_t
                            | Ok (Exponent(lhs_t, Exponent(rhs_t, out_t)) as binop_t), _
                                -> Error (sprintf "Type mismatch in binary operation : expected type %s but given type %s" (binop_t.ToString()) (Exponent(loperand_t, Exponent(roperand_t, out_t)).ToString())) 
                            | _ as error, _ -> error
                        typeOfExpr
                    else Error (sprintf "Undefined binary operation %s" symbol)
                | _ -> Error "Unsupported binary operation"
            handle op left_t right_t, ctx
        | Context (_, program) -> TypeOf program ctx


        // Statement * ((Statement * Type) * Statement) list