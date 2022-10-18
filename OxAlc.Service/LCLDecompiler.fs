module LCLDecompiler

    open Interpreter
    open Typechecker
    open Typedefinitions
    open Parsec
    open OxalcParser
    open System.IO
    open System.Text.RegularExpressions

    // let id = value in cont :: ((id) => cont)(value)

    let rec decompile lambdaAst  = 
        match lambdaAst with 
        | Term name -> Identifier name
        | Applicative (Lambda(varname, continuation), value) 
            when varname = continuation ->
            decompile value
        | Applicative (Lambda(varname, continuation), value) ->
            Bind (decompile varname, Atom "_", decompile value, decompile continuation) 
        | Applicative (action, arg) -> 
            Application (decompile action, [decompile arg])  
        | Lambda(param, body) -> // check if body is lambda and try to merge the lambda's sequence into 1 multi args functions
            Function ([decompile param, Atom "_"], decompile body)  
