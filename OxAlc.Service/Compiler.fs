module OxalcCompiler

    open Interpreter
    open Typechecker
    open Typedefinitions
    open Parsec
    open OxalcParser
    open System.IO
    open System.Text.RegularExpressions

    let transpile backend rawInput  = 
        match backend with 
        | LCR -> 
            let input = 
                match rawInput with 
                | Comment "\(\*.*?\*\)" comments ->
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
            match Result with
            | Success (program,r) -> 
                let typeResult = TypeOf Map.empty program 
                match typeResult with
                | Ok(expr_type) -> 
                    printf "val it : %s = " (expr_type.ToString())
                    let toSyntaxTree =  Interpreter.parse     >> (function Success(code,_) -> code) >> 
                                        Interpreter.interpret >> (function Ok (program)    -> program)
                    let rec emitLambda= function
                        | Context(files, program) ->
                            let filesContents = files  |> List.map (function Identifier(path) 
                                                                                                        -> path |> (sprintf "%s.oxalc") 
                                                                                                                |> File.ReadAllText 
                                                                                                                |> parseExp)
                            let all xs = 
                                let folder = fun state next -> 
                                    match (state, next) with 
                                    | (Ok ys, Success (n, s)) -> ys |> List.append [ n ] |> Ok
                                    | _  -> Error "File import Failed"
                                Seq.fold folder (Ok []) xs
                            let rec wrapFiles filesAst program = 
                                match filesAst with 
                                | []   -> program
                                | Bind(n, _t , f, e)::t ->
                                    Bind(n, _t, f, Application(n, [wrapFiles t program]))
                            match all filesContents with
                            | Ok (Asts) ->   
                                let r = wrapFiles (Asts) program
                                printfn "%A" r
                                emitLambda r
                            | Error(msg) -> failwith msg
                        | Bind(name,type_t,  expr, value) ->
                            Applicative(Lambda(emitLambda name, emitLambda value), emitLambda expr)
                        | Compound(expr, binds) as e -> 
                            let rec emitBinds binds = 
                                match binds with 
                                | [] -> emitLambda expr
                                | bind::binds ->
                                    let ((var, var_t), value) = bind 
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
                    Success (emitLambda program, r)
                | Error(msg) -> failwith msg
            | Failure(l, e, idx) -> Failure(l,e,idx)
        | _ -> failwith "Backend not supported : Yet" 