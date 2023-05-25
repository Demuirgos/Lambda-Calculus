module OxalcCompiler

    open Interpreter
    open Typechecker
    open Typedefinitions
    open Parsec
    open OxalcParser
    open System
    open System.IO
    open System.Text.RegularExpressions

    let transpile backend rawInput  = 
        let input = 
            match rawInput with 
            | Comment(comments) ->
                let rec removeComments (str:string) (comments: Capture list) =  
                    match comments with
                    | [] -> str
                    | h::t -> removeComments (str.Remove(h.Index, h.Length)) t
                removeComments rawInput comments
            | _ -> rawInput
        let rec curry =
            function 
            | Function ([_] , _ ) as input -> input
            | Function ((h::t),body) -> Function ([h], curry <| Function(t, body))
            | _ -> failwith "Expression cannot be curried"
        let parseExp arg = (fromStr arg, parseExpr) ||> run 
        let Result = parseExp input

        let getTypeOf name ctx = 
            let symbolType = Map.find name ctx.Symbols
            match symbolType with 
            | Atom id when Map.containsKey id ctx.Types
                ->  let typeOfType = Map.find id ctx.Types 
                    if typeOfType = Atom "type" then Atom id else typeOfType
            |_ -> symbolType


        printfn "%A" Result

        let rec injectFields fields p = 
            match fields with 
            | (fd_id, fd_t, fd_body)::t -> 
                Bind(fd_id, fd_t, fd_body, injectFields t p)
            | _ -> p

        match backend, Result with 
        | LCR, Ok (program,r)-> 
            let toSyntaxTree =  Interpreter.parse     >> (function Ok (LCRR(code),_)   -> code) >> 
                                Interpreter.interpret >> (function Ok (program)  -> program)
            let rec emitLambda ctx = function
                | Context(files, program) ->
                    let program = 
                        match files with
                        | [] -> Ok program
                        | _  ->  
                            let filesContents = List.map ( function Identifier(path) -> path, path |> ((sprintf "%s.oxalc") >> File.ReadAllText >> parseExp))
                            let all = 
                                let folder = fun state (file, next) -> 
                                    match (state, next) with 
                                    | (Ok ys, Ok (n, _)) -> ys @ [n] |> Ok
                                    | Error errAcc, Error errNew  -> Error (errNew::errAcc)
                                    | Error _ as error, _ -> error 
                                    | _, Error err  -> Error [err]
                                Seq.fold folder (Ok []) 

                            let rec wrapFiles filesAst program = 
                                match filesAst with 
                                | []   -> Ok program
                                | Bind(_, _ , Context(dependencies, Value(Record(fields))), _)::t ->
                                    let dependenciesContent = all (filesContents dependencies)
                                    match dependenciesContent with
                                    | Ok ds -> 
                                        match wrapFiles ds (injectFields fields program) with
                                        | Ok result -> wrapFiles t result
                                        | Error err -> Error err
                                    | Error msg -> Error msg
                                | Bind(_, _ , Value(Record(fields)), _)::t ->
                                    wrapFiles t (injectFields fields program)
                                | _ -> Error [("Import Error","Invalid file structure", None)]
                            match all (filesContents files) with
                            | Ok (Asts)-> wrapFiles (Asts) program 
                            | Error err -> Error err
                    match program with
                    | Ok program -> 
                        let typeResult = TypeOf program TypingContext.Empty
                        match typeResult with
                        | Ok(expr_type), ctx -> 
                            printf "val it : %s = " (expr_type.ToString())
                            emitLambda (Some ctx) program 
                        | Error msg, _ -> failwith msg
                    | Error msgs -> 
                        let rec msgAcc msgs = 
                            match msgs with
                            | [(label, msg, _)] -> sprintf "%s: %s" label msg
                            | (label, msg, _)::errs -> 
                                sprintf "%s: %s \n%s" label msg (msgAcc errs)
                        failwith (msgAcc msgs)
                | Bind(name,_,  expr, value) ->
                    match expr with 
                    | Value(TypeDefinition(type_instance)) -> emitLambda ctx value
                    | _ -> Applicative(Lambda(emitLambda ctx name, emitLambda ctx value), emitLambda ctx expr)
                | Compound(expr, binds) as e -> 
                    let rec emitBinds ctx binds = 
                        match binds with 
                        | [] -> emitLambda ctx expr
                        | ((var, _), value) ::binds ->
                            Applicative(Lambda(emitLambda ctx var, emitBinds ctx binds), emitLambda ctx value)
                    emitBinds ctx binds
                | Function _ as f->
                    let emitFunction (Function([param], body)) = match param with 
                        | (n, t)    -> Lambda(emitLambda ctx n, emitLambda ctx body)
                    f |> curry |> emitFunction
                | Application(expr, args) as a ->
                    let operation = emitLambda ctx expr
                    let rec wrap op = function
                        | [] -> op
                        | h::t -> wrap (Applicative(op, (emitLambda ctx h))) t
                    wrap operation args
                | Identifier(name) -> Term name
                | Unary(Op, rhs) -> 
                    match Op with 
                    | Not -> Applicative(Applicative(emitLambda ctx rhs, emitLambda ctx (Value (Bool false))), emitLambda ctx (Value (Bool true)))
                    | YComb ->
                        let Y = "\\_g.(\\_y.(_g (_y _y)) \\_y.(_g (_y _y)))" |> toSyntaxTree
                        Applicative(Y, (emitLambda ctx rhs))
                    | _ -> failwith "Unary operator not supported" 
                | Branch(cond,tClause, fClause) as (t: Statement) -> 
                    Applicative (Applicative(emitLambda ctx cond, emitLambda ctx tClause), emitLambda ctx fClause)
                | Binary(lhs, op, rhs) ->
                    let isZero arg = Applicative(Applicative(arg,Lambda(Term "_w", emitLambda ctx (Value (Bool false)))), emitLambda ctx (Value (Bool true)))
                    let predec = "\\_n.\\_f.\\_x.(((_n \\_g.\\_h.(_h (_g _f))) \\_u._x) \\_u._u)" |> toSyntaxTree
                    match op  with 
                    | Add -> Lambda(Term "_g", Lambda(Term "_v", Applicative(Applicative(emitLambda ctx lhs, Term "_g"), Applicative(Applicative(emitLambda ctx rhs, Term "_g"), Term "_v"))))
                    | Mult-> Lambda(Term "_g", Lambda(Term "_v", Applicative(Applicative(emitLambda ctx lhs, Applicative(emitLambda ctx rhs, Term "_g")), Term "_v")))
                    | Exp -> Applicative(emitLambda ctx rhs, emitLambda ctx lhs)
                    | And -> Applicative(Applicative(emitLambda ctx rhs, emitLambda ctx lhs), emitLambda ctx (Value (Bool false)))
                    | Or  -> Applicative(Applicative(emitLambda ctx rhs, emitLambda ctx (Value (Bool false))), emitLambda ctx lhs)
                    | Subs-> Applicative(Applicative(emitLambda ctx rhs, predec), emitLambda ctx lhs)
                    | Lt  -> isZero (emitLambda ctx (Binary(lhs, Subs, rhs))) | Gt  -> emitLambda ctx (Binary(rhs, Lt, lhs))
                    | Eq  -> emitLambda ctx (Binary(Binary(lhs, Lt, rhs), And, Binary(lhs, Gt, rhs)))
                    | Xor -> Applicative(Applicative(emitLambda ctx rhs, emitLambda ctx (Unary(Not, lhs))), emitLambda ctx lhs)
                    | Custom(token) -> emitLambda ctx (Application(Identifier token, [lhs; rhs]))
                    | Cons -> emitLambda ctx (Value (List [lhs; rhs]))
                    | Div | Not | YComb -> failwith "Not Implemented"
                | Value(var) -> 
                    match var with 
                    | List(elems) -> 
                        let cons = Lambda(Term "_h", Lambda(Term "_t", Lambda(Term "_s", Applicative(Applicative(Term "_s", Term "_h"), Term "_t"))))
                        let empty= Lambda(Term "_l", Applicative(Term "_l", Lambda(Term "_h", Lambda(Term "_t", emitLambda ctx (Value (Bool false))))))
                        let nil  = Lambda(Term "_l", Applicative(Term "_l", emitLambda ctx (Value (Bool true))))
                        let rec emitList = function 
                            | []   -> nil
                            | h::t -> Applicative(Applicative(cons, emitLambda ctx h), emitList t)
                        emitList elems
                    | Bool booleanExpr -> 
                        if booleanExpr = true 
                        then Lambda(Term "_a", Lambda(Term "_b", Term "_a"))
                        else Lambda(Term "_a", Lambda(Term "_b", Term "_b"))
                    | Variable(var) ->
                        let funcn, varn = "_a", "_b"
                        let rec loop n = 
                            match n with 
                            | 0 -> Term varn
                            | _ -> Applicative(Term funcn, (loop (n - 1)))
                        Lambda(Term funcn, Lambda(Term varn, loop var))
                    | _ as v -> failwithf "%A is not supported by Lambda-Calculus" v 
                | _  -> failwith "Not Implemented or Not Compilable"
            Ok (LCRR(emitLambda None program), ``initial State``)
        | JS, Ok (program,r) -> 
            let rec emitJavascript ctx = function
                | Context(files, program) ->
                    let program = 
                        match files with
                        | [] -> Ok program
                        | _  ->  
                            let filesContents = List.map ( function Identifier(path) -> path, path |> ((sprintf "%s.oxalc") >> File.ReadAllText >> parseExp))
                            let all = 
                                let folder = fun state (file, next) -> 
                                    match (state, next) with 
                                    | (Ok ys, Ok (n, _)) -> ys @ [n] |> Ok
                                    | Error errAcc, Error errNew  -> Error (errNew::errAcc)
                                    | Error _ as error, _ -> error 
                                    | _, Error err  -> Error [err]
                                Seq.fold folder (Ok []) 

                            let rec wrapFiles filesAst program = 
                                match filesAst with 
                                | []   -> Ok program
                                | Bind(_, _ , Context(dependencies, Value(Record(fields))), _)::t ->
                                    match all (filesContents dependencies) with
                                    | Ok ds -> 
                                        match wrapFiles ds (injectFields fields program) with
                                        | Ok result -> wrapFiles t result
                                        | Error err -> Error err
                                    | Error msg -> Error msg
                                | Bind(_, _ , Value(Record(fields)), _)::t ->
                                    wrapFiles t (injectFields fields program)
                                | _ -> Error [("Import Error","Invalid file structure", None)]
                            match all (filesContents files) with
                            | Ok (Asts)-> wrapFiles (Asts) program 
                            | Error err -> Error err
                    match program with
                    | Ok program -> 
                        let typeResult = TypeOf program TypingContext.Empty
                        match typeResult with
                        | Ok typeName, ctx -> 
                            printfn "%A" ctx
                            printf "var it = " 
                            emitJavascript (Some ctx) program 
                        | Error msg, _ -> failwith msg
                    | Error msgs -> 
                        let rec msgAcc msgs = 
                            match msgs with
                            | [(label, msg, _)] -> sprintf "%s: %s" label msg
                            | (label, msg, _)::errs -> 
                                sprintf "%s: %s \n%s" label msg (msgAcc errs)
                        failwith (msgAcc msgs)
                | Bind(Identifier(id) as name,_,  expr, value) ->
                    match expr, ctx with 
                    | Value(TypeDefinition(type_instance)), _ -> emitJavascript ctx value
                    | Binary _, Some ctx_inner when (Map.find id ctx_inner.Symbols) = Atom "type" -> emitJavascript ctx value
                    | _ -> 
                        if name = value then 
                            sprintf "%s" (emitJavascript ctx expr)
                        else 
                            sprintf "((%s) => %s)(%s)" (emitJavascript ctx name) (emitJavascript ctx value) (emitJavascript ctx expr)
                | Compound(expr, binds) as e -> 
                    let rec hoistBinds binds =
                        match binds with 
                        | [((name, t), body)] -> Bind(name, t, body, expr)
                        | ((name, typ), body)::t -> Bind(name, typ, body, hoistBinds t)
                    sprintf "%s" (emitJavascript ctx <| hoistBinds binds) 
                | Function _ as f->
                    match curry f with
                    | Function([(arg, _)], body) -> 
                        let (ftype, _) = TypeOf f ctx.Value
                        match ftype with 
                        | Ok ftype_r -> sprintf "({ value:(%s) => %s, type:\"%s\" })" (emitJavascript ctx arg) (emitJavascript ctx body) (ftype_r.ToString()) 
                | Application(expr, args) as a ->
                    let isIdentity = 
                        match expr with 
                        | Function([(iden, _)], body) -> iden = body
                        | _ -> false
                    if isIdentity then 
                        sprintf "%s" (emitJavascript ctx (List.head args))
                    else
                        let rec prepareArgs ctx params = 
                            params 
                            |> List.map (fun p -> sprintf "(%s)" (emitJavascript ctx p))
                            |> List.fold (fun acc p -> sprintf "%s %s" acc p) ""
                        sprintf "(%s).value%s" (emitJavascript ctx expr) (prepareArgs ctx args)
                | Identifier(name) ->
                    let replaceSpecialCharacters c = 
                        match c with 
                        | '!' -> "not"
                        | '#' -> "diz"
                        | '$' -> "dol"
                        | '%' -> "mod"
                        | '&' -> "and"
                        | '*' -> "mul"
                        | '+' -> "add"
                        | '-' -> "sub"
                        | '.' -> "dot"
                        | '/' -> "div"
                        | '<' -> "grt"
                        | '=' -> "eql"
                        | '>' -> "let"
                        | '?' -> "qst"
                        | '\\'-> "dsh"
                        | '^' -> "hat"
                        | '|' -> "ord"
                        | '~' -> "tld"
                        | c    -> c.ToString()
                    let updateName = 
                        name
                        |> Seq.map replaceSpecialCharacters
                        |> Seq.fold (fun acc c -> acc + c) ""
                    sprintf "%s" updateName
                | Unary(Op, rhs) ->
                    let opToString op = match op with 
                        | Not -> "!"
                        | Subs -> "-"
                        | _ -> failwith "Not Implemented" 
                    sprintf "%s%s" (opToString Op) (emitJavascript ctx rhs) 
                | Branch(cond,tClause, fClause) as (t: Statement) -> 
                    sprintf "((%s.value) ? (() => %s) : (() => %s))()" (emitJavascript ctx cond) (emitJavascript ctx tClause) (emitJavascript ctx fClause)
                | Binary(lhs, Op, rhs) as binop->
                    let (btype, _) = TypeOf binop ctx.Value
                    let isFieldLens = 
                        match rhs with 
                        | Identifier _ -> true
                        | _ -> false

                    let opToString op = 
                        match op with 
                        | Add  -> "+"
                        | Subs -> "-"
                        | Mult -> "*"
                        | Div  -> "/"
                        | Eq   -> "=="
                        | Neq  -> "!="
                        | Lt   -> "<"
                        | Gt   -> ">"
                        | And  -> "&&"
                        | Or   -> "||"
                        | _ -> String.Empty
                    match Op, btype with
                    | Dot, _ when isFieldLens -> sprintf "%s.value.%s" (emitJavascript ctx lhs) (emitJavascript ctx rhs)
                    | Dot, _  -> sprintf "%s.value[%s]" (emitJavascript ctx lhs) (emitJavascript ctx rhs)
                    | Custom op, _ -> sprintf "%s.value(%s)(%s)" (emitJavascript ctx (Identifier op)) (emitJavascript ctx lhs) (emitJavascript ctx rhs)
                    | _, Ok(btype_r)  -> sprintf "({ value:(%s.value %s %s.value), type: \"%s\"})" (emitJavascript ctx lhs) (opToString Op) (emitJavascript ctx rhs) (btype_r.ToString())
                | Value(var) as variable-> 
                    let (vtype, _) = TypeOf variable ctx.Value 
                    let varToString = match var with 
                        | Variable(n) -> 
                            sprintf "({ value:%s, type:\"%s\" })" (n.ToString()) ("number")
                        | Bool(b) -> 
                            sprintf "({ value:%s, type:\"%s\" })" (b.ToString().ToLower()) ("bool")
                        | String(str) -> 
                            sprintf "({ value:%s, type:\"%s\" })" (sprintf "\"%s\"" (toString str)) ("word")
                        | Tuple(values) | List(values) -> 
                            let value = sprintf "[%s]" (String.concat "," (List.map (emitJavascript ctx) values))
                            match vtype with 
                            | Ok vtype_r -> sprintf "({ value:%s, type:\"%s\" })" (value) (vtype_r.ToString())
                        | Record(fields) -> 
                            let rec fieldsToString fields = 
                                match fields with 
                                | [] -> System.String.Empty
                                | ((name, _, value)::t) -> sprintf "\t%s: %s,\n%s" (emitJavascript ctx name) (emitJavascript ctx value) (fieldsToString t)
                            let value = sprintf "{\n%s}" (fieldsToString fields)
                            match vtype with 
                            | Ok vtype_r -> sprintf "({ value:%s, type:\"%s\" })" (value) (vtype_r.ToString())
                    sprintf "%s" varToString
                | Match(Identifier(id) as argument, pats) -> 
                    let getInputType t = 
                        match t with 
                        | Ok(Exponent(input_t, _)) -> Ok input_t
                    // lazy way cause lazy ~\_(*-*)_/~
                    let rec matchBody  = 
                        function
                        | [(pat, Ok(typeOfHead))] -> 
                            sprintf "((\"%s\" === %s.type) ? (%s.value(%s)) :  (function(){throw \"unhandled case was encountered in match\"}()))" (typeOfHead.ToString()) (id)  (emitJavascript ctx pat) (id)
                        | (pat, Ok(typeOfHead))::t -> 
                            sprintf "((\"%s\" === %s.type) ? (%s.value(%s)) : (%s))" (typeOfHead.ToString()) (id) (emitJavascript ctx pat) (id) (matchBody t) 
                    matchBody (pats |> List.map (fun pat -> pat, (getInputType << fst) <| TypeOf pat ctx.Value))
                | _  -> failwith "Not Implemented or Not Compilable"
            Ok (JSR(emitJavascript None program), ``initial State``)
        | target, Ok _ -> Error("Backend not supported", sprintf "%A is not supported" target, None)
        | _, Error err-> Error err

// type fullname = { lastname:word; firstname:word; } in type student = { name:fullname; age:number; } in let ayman_obj := { name:= { lastname:= "ayman"; firstname:= "bouchareb" }; age:=23 } in ayman_obj