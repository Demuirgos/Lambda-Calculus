module WEBGenerator

    open Interpreter
    open Typechecker
    open Typedefinitions
    open Parsec
    open OxalcParser
    open System.IO
    open System.Text.RegularExpressions

    let emitJS Result parseExp curry = 
        match Result with 
        | Ok (program,r) -> 
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
                | Bind(name,_,  expr, value) when value <> name ->
                    sprintf "((%s)=>%s)(%s)" (emitJavascript name) (emitJavascript value) (emitJavascript expr)
                | Bind(name,_,  expr, value) when value = name -> emitJavascript expr
                | Compound(expr, binds) as e -> 
                    let rec hoistBinds binds =
                        match binds with 
                        | [((name, t), body)] -> Bind(name, t, body, expr)
                        | ((name, typ), body)::t -> Bind(name, typ, body, hoistBinds t)
                    sprintf "%s" (emitJavascript <| hoistBinds binds) 
                | Function _ as f->
                    match curry f with
                    | Function([(arg, _)], body) -> 
                        sprintf "(%s)=>%s" (emitJavascript arg) (emitJavascript body) 
                | Application(expr, args) as a ->
                    let rec prepareArgs params = 
                        params 
                        |> List.map (fun p -> sprintf "(%s)" (emitJavascript p))
                        |> List.fold (fun acc p -> sprintf "%s%s" acc p) ""
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
                    sprintf "((%s)?(()=>%s):(()=>%s))()" (emitJavascript cond) (emitJavascript tClause) (emitJavascript fClause)
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
                    | Ok op -> sprintf "%s%s%s" (emitJavascript lhs) op (emitJavascript rhs)
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
        | Error err-> Error err