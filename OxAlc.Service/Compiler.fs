module OxalcCompiler

    open Interpreter
    open LCLGenerator
    open WEBGenerator
    open CLRGenerator
    open LCLDecompiler
    open EVMGenerator
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
        | LCR , (Ok _ as program) -> 
            let result = emitLC program parseExp curry
            match result with 
            | Ok (LCRR code,_) -> 
                let decompiledCode = decompile code
                printf "%s\n" (sprintOxalcAST decompiledCode)
            | _ -> ()
            result
        | JS  , (Ok _ as program) -> emitJS program parseExp curry
        | EVM , (Ok _ as program) -> emitIR program parseExp curry
        | MSIL, (Ok _ as program) -> emitIL program parseExp curry

        | target, Ok _ -> Error("Backend not supported", sprintf "%A is not supported" target, None)
        | _, Error err-> Error err