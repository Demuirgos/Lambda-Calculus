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
            Symbols = Map.add name (Atom "type") ctx.Symbols
            Types = Map.add name type_n ctx.Types  
    }

    let rec actualTypeOf type_name ctx = 
        match type_name with
        | Atom iden -> 
            match Map.tryFind iden ctx.Types with 
            | Some(type_def) when type_def <> Atom "type"-> 
                actualTypeOf type_def ctx
            | Some(type_def) -> 
                type_def
            | None -> type_name
        | _ -> type_name
    
    let rec isTypeMatch type_t type_v (ctx:TypingContext) = 
        let rec checkTypeMatch type_t type_v ctx= 
            match type_t with 
            | _ when type_t = type_v -> true
            | Union(types) ->
                0 < (List.length <| List.where (fun t -> checkTypeMatch t type_v ctx) types)
            | Atom(identifier) when identifier <> "type" ->
                match Map.tryFind identifier ctx.Types with 
                | None -> false
                | Some(type_def) -> checkTypeMatch type_def type_v ctx
            | _ -> false
        checkTypeMatch type_t (actualTypeOf type_v ctx) ctx



    let rec flattenIfUnionType type_def includeLiterals ctx =
        let rec isUnion t = 
            match t with 
            | Union _ -> true, t
            | Atom name as lit 
                when name <> "type" 
                    && includeLiterals 
                -> 
                    let actualTypeRef = actualTypeOf lit ctx
                    if actualTypeRef = lit then 
                        false, lit 
                    else isUnion actualTypeRef
            | _ -> false, t
        // lazy way Todo : improve algorithm or use a better one
        let rec loop ts =
            if not (List.forall (not << fst << isUnion) ts) then 
                loop (List.concat <| List.map (fun type_name -> match isUnion type_name with 
                    | _, Union(ts) -> ts
                    | _ -> [type_name]
                ) ts)
            else ts
        match type_def with 
        | Union(ts) -> Union(loop ts)
        | Atom _ -> 
            let (isUnion, actualType) = isUnion type_def 
            if isUnion then flattenIfUnionType actualType includeLiterals ctx 
            else type_def

    let flattenResults results =
        let rec loop flatResult results =
            match results with 
            | [] -> Ok (flatResult)
            | Ok a :: t -> loop (a::flatResult) t
            | (Error err as error) :: _  -> Error(err)
        loop [] results

    let rec addRange l ctx: TypingContext = 
        match l with 
        | [] -> ctx
        | (Identifier(name), arg_t):: t -> addRange t (addSymbol name arg_t ctx)
        | _ -> failwith "Invalid type definition"
    
    // check suggested types if they alias type infered
    let rec TypeOf term (ctx : TypingContext) : (Result<Type, string> * TypingContext) = 
        let actualTypeOf term ctx = 
            let (type_name_r, ctx) = TypeOf term ctx
            match type_name_r with 
            | Ok (Atom type_name) as type_found -> 
                let actual_type = Map.find type_name ctx.Types 
                if actual_type <> Atom "type"then 
                    Ok actual_type, ctx
                else type_found, ctx
            | _ -> type_name_r, ctx

        match term with 
        | Bind(Identifier(name), suggested_type, body, cont) -> 
            let (bodyType, ctx) = TypeOf body ctx  
            match suggested_type, bodyType with
            | _ , Ok(term_t) 
                when isTypeMatch suggested_type term_t ctx || suggested_type = Atom String.Empty ->
                    let ctx = 
                        match term_t, body with 
                        | Atom "type", Value(TypeDefinition(type_def)) ->
                            addType name (flattenIfUnionType type_def false ctx) ctx
                        | Atom "type", Binary(Identifier(lhs), op, Identifier(rhs)) ->
                            match op with 
                            | And -> addType name (Intersection [Atom lhs; Atom rhs]) ctx
                            | Or  -> addType name (flattenIfUnionType (Union [Atom lhs; Atom rhs]) false ctx) ctx
                        | _ -> 
                            addSymbol name (if suggested_type = Atom String.Empty
                                then term_t 
                                else suggested_type)  ctx
                    TypeOf cont ctx 
            | Atom(identifier), Ok(term_t) when Map.containsKey identifier ctx.Types && term_t = (Map.find identifier ctx.Types) ->
                TypeOf cont (addSymbol name suggested_type ctx)  
            | _, Ok(term_t) -> Error (sprintf "Type mismatch in bind : expected type %s but given type %s" (suggested_type.ToString()) (term_t.ToString())), ctx
            | _, error -> error, ctx
        | Match(_identifier, patterns, fallthrough) -> 
            let (arg_type_r, ctx) = actualTypeOf _identifier ctx
            let pats_type_r =  
                patterns 
                |> List.map (fun pat -> fst <| TypeOf pat ctx)
                |> flattenResults

            let rec match_expression_sig pats_types type_i type_r=
                match pats_types, type_i, type_r with 
                | [], Some itype_defs, Some rtype_def -> (Some itype_defs), (Some rtype_def)
                | Exponent(in_type, ret_type)::r, None, None -> match_expression_sig r (Some [in_type]) (Some [ret_type])
                | Exponent(in_type, ret_type)::r, Some t_i, Some t_r -> match_expression_sig r (Some (in_type::t_i)) (Some (ret_type::t_r))
            
            match arg_type_r, pats_type_r with 
            | Ok(Atom arg_type as arg_type_w), Ok(pats_types) -> 
                match fallthrough, match_expression_sig pats_types None None with 
                | None, (Some types, Some rtypes) when Map.containsKey arg_type ctx.Types -> 
                    match Map.find arg_type ctx.Types with 
                    | Union(possible_types) when List.forall (fun typ -> List.contains typ types) possible_types ->
                        Ok (Union rtypes), ctx
                    | simple_type when simple_type = Atom "type" && List.contains arg_type_w types->
                        Ok (Union rtypes), ctx
                    | simple_type when simple_type <> Atom "type" && List.contains simple_type types->
                        Ok (Union rtypes), ctx
                    | _ -> Error (sprintf "type mismatch"), ctx
                | None, (Some _, Some rtypes) -> 
                    Ok (Union rtypes), ctx
                | _ -> Error (sprintf "type mismatch"), ctx
            | Ok(arg_type), Ok(pats_type_) -> 
                match flattenIfUnionType arg_type true ctx with 
                | Union(arg_possible_types) -> 
                    match return_type pats_type_ None None with 
                    | Some types, Ok rtype when List.forall (fun arg_t -> List.contains arg_t types) arg_possible_types -> Ok rtype, ctx
                    | _ -> Error (sprintf "type mismatch"), ctx
                | Atom single_type as arg_type-> 
                    match return_type pats_type_ None None with 
                    | Some types, Ok rtype when List.contains arg_type types -> Ok rtype, ctx
                    | _ -> Error (sprintf "type mismatch"), ctx

            | Error err1, Error err2 -> Error (sprintf "%s; %s" err1 err2), ctx
            | Error err, _ -> Error err, ctx
            | _ , Error err -> Error err, ctx
            | _ -> Error "type mismatch", ctx
        | Branch(_cond, _then, _else) -> 
            let (condType, ctx) = TypeOf _cond ctx 
            let (thenType, ctx) = TypeOf _then ctx 
            let (elseType, ctx) = TypeOf _else ctx  
            match condType, thenType, elseType with
            | Ok(Atom "bool"), Ok(then_t), Ok(else_t) when then_t = else_t -> Ok(then_t), ctx
            | Ok(Atom "bool"), Ok(then_t), Ok(else_t) when then_t <> else_t -> Ok(Union [then_t; else_t]), ctx
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
                | Ok (Exponent(Atom(identifier) as in_t, out_t)), (Ok t):: ts -> 
                    match Map.tryFind identifier ctx.Types with 
                    | Some(Union p_ts as desired_type) when t = desired_type || List.contains t p_ts -> checkValidity (Ok out_t) ts
                    | _ -> Error (sprintf "Type mismatch in application : expected type %s but given type %s" (in_t.ToString()) (t.ToString()))
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
                | And , Ok (Atom "type"  ) , Ok(Atom "type"  ) -> Ok (Atom "type")
                
                | Or  , Ok (Atom "bool"  ) , Ok(Atom "bool"  ) -> Ok (Atom "bool")
                | Or  , Ok (Atom "type"  ) , Ok(Atom "type"  ) -> Ok (Atom "type")

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