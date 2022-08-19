module OxalcCompiler

    open Interpreter
    open Typechecker
    open Typedefinitions
    open Parsec
    open OxalcParser
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
        match backend, Result with 
        | LCR, Ok (program,r)-> 
            let toSyntaxTree =  Interpreter.parse     >> (function Ok (LCRR(code),_)   -> code) >> 
                                Interpreter.interpret >> (function Ok (program)  -> program)
            let rec emitLambda= function
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

                            let rec injectFields fields p= 
                                match fields with 
                                | (fd_id, fd_t, fd_body)::t -> 
                                    Bind(fd_id, fd_t, fd_body, injectFields t p)
                                | _ -> p

                            let rec wrapFiles filesAst program = 
                                match filesAst with 
                                | []   -> Ok program
                                | Bind(_, _ , Context(dependencies, Library(fields)), _)::t ->
                                    let dependenciesContent = all (filesContents dependencies)
                                    match dependenciesContent with
                                    | Ok ds -> 
                                        match wrapFiles ds (injectFields fields program) with
                                        | Ok result -> wrapFiles t result
                                        | Error err -> Error err
                                    | Error msg -> Error msg
                                | Bind(_, _ , Library(fields), _)::t ->
                                    wrapFiles t (injectFields fields program)
                                | _ -> Error [("Import Error","Invalid file structure", None)]
                            match all (filesContents files) with
                            | Ok (Asts)-> wrapFiles (Asts) program 
                            | Error err -> Error err
                    match program with
                    | Ok program -> 
                        let typeResult = TypeOf Map.empty program 
                        match typeResult with
                        | Ok(expr_type) -> 
                            printf "val it : %s = " (expr_type.ToString())
                            emitLambda program
                        | Error msg -> failwith msg
                    | Error msgs -> 
                        let rec msgAcc msgs = 
                            match msgs with
                            | [(label, msg, _)] -> sprintf "%s: %s" label msg
                            | (label, msg, _)::errs -> 
                                sprintf "%s: %s \n%s" label msg (msgAcc errs)
                        failwith (msgAcc msgs)
                | Bind(name,_,  expr, value) ->
                    Applicative(Lambda(emitLambda name, emitLambda value), emitLambda expr)
                | Compound(expr, binds) as e -> 
                    let rec emitBinds binds = 
                        match binds with 
                        | [] -> emitLambda expr
                        | ((var, _), value) ::binds ->
                            Applicative(Lambda(emitLambda var, emitBinds binds), emitLambda value)
                    emitBinds binds
                | Function _ as f->
                    let emitFunction (Function([param], body)) = match param with 
                        | (n, t)    -> Lambda(emitLambda n, emitLambda body)
                    f |> curry |> emitFunction
                | Application(expr, args) as a ->
                    let operation = emitLambda expr
                    let rec wrap op = function
                        | [] -> op
                        | h::t -> wrap (Applicative(op, (emitLambda h))) t
                    wrap operation args
                | Identifier(name) -> Term name
                | Unary(Op, rhs) -> 
                    match Op with 
                    | Not -> Applicative(Applicative(emitLambda rhs, emitLambda (Value (Bool false))), emitLambda (Value (Bool true)))
                    | YComb ->
                        let Y = "\\_g.(\\_y.(_g (_y _y)) \\_y.(_g (_y _y)))" |> toSyntaxTree
                        Applicative(Y, (emitLambda rhs))
                    | _ -> failwith "Unary operator not supported" 
                | Branch(cond,tClause, fClause) as (t: Statement) -> 
                    Applicative (Applicative(emitLambda cond, emitLambda tClause), emitLambda fClause)
                | Binary(lhs, op, rhs) ->
                    let isZero arg = Applicative(Applicative(arg,Lambda(Term "_w", emitLambda (Value (Bool false)))), emitLambda (Value (Bool true)))
                    let predec = "\\_n.\\_f.\\_x.(((_n \\_g.\\_h.(_h (_g _f))) \\_u._x) \\_u._u)" |> toSyntaxTree
                    match op  with 
                    | Add -> Lambda(Term "_g", Lambda(Term "_v", Applicative(Applicative(emitLambda lhs, Term "_g"), Applicative(Applicative(emitLambda rhs, Term "_g"), Term "_v"))))
                    | Mult-> Lambda(Term "_g", Lambda(Term "_v", Applicative(Applicative(emitLambda lhs, Applicative(emitLambda rhs, Term "_g")), Term "_v")))
                    | Exp -> Applicative(emitLambda rhs, emitLambda lhs)
                    | And -> Applicative(Applicative(emitLambda rhs, emitLambda lhs), emitLambda (Value (Bool false)))
                    | Or  -> Applicative(Applicative(emitLambda rhs, emitLambda (Value (Bool false))), emitLambda lhs)
                    | Subs-> Applicative(Applicative(emitLambda rhs, predec), emitLambda lhs)
                    | Lt  -> isZero (emitLambda (Binary(lhs, Subs, rhs))) | Gt  -> emitLambda (Binary(rhs, Lt, lhs))
                    | Eq  -> emitLambda (Binary(Binary(lhs, Lt, rhs), And, Binary(lhs, Gt, rhs)))
                    | Xor -> Applicative(Applicative(emitLambda rhs, emitLambda (Unary(Not, lhs))), emitLambda lhs)
                    | Custom(token) -> emitLambda(Application(Identifier token, [lhs; rhs]))
                    | Cons -> emitLambda (Value (List [lhs; rhs]))
                    | Div | Not | YComb -> failwith "Not Implemented"
                | Value(var) -> 
                    match var with 
                    | List(elems)-> 
                        let cons = Lambda(Term "_h", Lambda(Term "_t", Lambda(Term "_s", Applicative(Applicative(Term "_s", Term "_h"), Term "_t"))))
                        let empty= Lambda(Term "_l", Applicative(Term "_l", Lambda(Term "_h", Lambda(Term "_t", emitLambda(Value (Bool false))))))
                        let nil  = Lambda(Term "_l", Applicative(Term "_l", emitLambda(Value (Bool true))))
                        let rec emitList = function 
                            | []   -> nil
                            | h::t -> Applicative(Applicative(cons, emitLambda h), emitList t)
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
            Ok (LCRR(emitLambda program), ``initial State``)
        | JS, Ok (program,r) -> 
            let rec emitJavascript= function
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

                            let rec injectFields fields p= 
                                match fields with 
                                | (fd_id, fd_t, fd_body)::t -> 
                                    Bind(fd_id, fd_t, fd_body, injectFields t p)
                                | _ -> p

                            let rec wrapFiles filesAst program = 
                                match filesAst with 
                                | []   -> Ok program
                                | Bind(_, _ , Context(dependencies, Library(fields)), _)::t ->
                                    let dependenciesContent = all (filesContents dependencies)
                                    match dependenciesContent with
                                    | Ok ds -> 
                                        match wrapFiles ds (injectFields fields program) with
                                        | Ok result -> wrapFiles t result
                                        | Error err -> Error err
                                    | Error msg -> Error msg
                                | Bind(_, _ , Library(fields), _)::t ->
                                    wrapFiles t (injectFields fields program)
                                | _ -> Error [("Import Error","Invalid file structure", None)]
                            match all (filesContents files) with
                            | Ok (Asts)-> wrapFiles (Asts) program 
                            | Error err -> Error err
                    match program with
                    | Ok program -> 
                        let typeResult = TypeOf Map.empty program 
                        match typeResult with
                        | Ok _ -> 
                            printf "var it = " 
                            emitJavascript program
                        | Error msg -> failwith msg
                    | Error msgs -> 
                        let rec msgAcc msgs = 
                            match msgs with
                            | [(label, msg, _)] -> sprintf "%s: %s" label msg
                            | (label, msg, _)::errs -> 
                                sprintf "%s: %s \n%s" label msg (msgAcc errs)
                        failwith (msgAcc msgs)
                | Bind(name,_,  expr, value) ->
                    sprintf "((%s) => %s)(%s)" (emitJavascript name) (emitJavascript value) (emitJavascript expr)
                | Compound(expr, binds) as e -> 
                    let rec hoistBinds binds =
                        match binds with 
                        | [((name, t), body)] -> Bind(name, t, body, expr)
                        | ((name, typ), body)::t -> Bind(name, typ, body, hoistBinds t)
                    sprintf "%s" (emitJavascript <| hoistBinds binds) 
                | Function _ as f->
                    match curry f with
                    | Function([(arg, _)], body) -> 
                        sprintf "(%s) => %s" (emitJavascript arg) (emitJavascript body) 
                | Application(expr, args) as a ->
                    let rec prepareArgs params = 
                        params 
                        |> List.map (fun p -> sprintf "(%s)" (emitJavascript p))
                        |> List.fold (fun acc p -> sprintf "%s %s" acc p) ""
                    sprintf "%s%s" (emitJavascript expr) (prepareArgs args)
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
                    sprintf "%s%s" (opToString Op) (emitJavascript rhs) 
                | Branch(cond,tClause, fClause) as (t: Statement) -> 
                    sprintf "((%s) ? (() => %s) : (() => %s))()" (emitJavascript cond) (emitJavascript tClause) (emitJavascript fClause)
                | Binary(lhs, Op, rhs) ->
                    let opToString op = 
                        match op with 
                        | Add  -> Ok "+"
                        | Subs -> Ok "-"
                        | Mult -> Ok "*"
                        | Div  -> Ok "/"
                        | Eq   -> Ok "=="
                        | Neq  -> Ok "!="
                        | Lt   -> Ok "<"
                        | Gt   -> Ok ">"
                        | And  -> Ok "&&"
                        | Or   -> Ok "||"
                        | Custom op -> Error op
                        | _ -> failwith "Not Implemented" 
                    match opToString Op with
                    | Ok op -> sprintf "%s %s %s" (emitJavascript lhs) op (emitJavascript rhs)
                    | Error op -> sprintf "%s(%s)(%s)" (emitJavascript(Identifier op)) (emitJavascript lhs) (emitJavascript rhs)
                | Value(var) -> 
                    let varToString = match var with 
                        | Variable(n) -> n.ToString()
                        | Bool(b) -> b.ToString().ToLower()
                        | String(str) -> toString str
                        | Record(fields) -> 
                            let rec fieldsToString fields = 
                                match fields with 
                                | [] -> System.String.Empty
                                | ((name, value)::t) -> sprintf "%s: %s,\n%s" (emitJavascript name) (emitJavascript value) (fieldsToString t)
                                | _ -> failwith "Not Implemented" 
                            sprintf "{%s}" (fieldsToString fields)
                        | _ -> failwith "Not Implemented"
                    sprintf "%s" varToString
                | _  -> failwith "Not Implemented or Not Compilable"
            Ok (JSR(emitJavascript program), ``initial State``)
        | target, Ok _ -> Error("Backend not supported", sprintf "%A is not supported" target, None)
        | _, Error err-> Error err