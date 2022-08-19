module OxAlc.Service.Library

open System
open System.IO
open FSharp.Core
open Interpreter
open Typedefinitions
open OxalcParser
open OxalcCompiler

type Mode = Lambda | Oxalc

let transpile = OxalcCompiler.transpile >> sprintf "%A"

let interpret = Interpreter.parse 
                >> function
                | Ok (LCRR(code), _) -> code |> Interpreter.interpret |> sprintf "%A"
                | _ as error -> error |> toResult
                
let decompile = Interpreter.parse 
                >> function
                // | Ok (code, _) -> code |> Abstractor.uncompile |> (sprintf "%A")
                | _ as error -> error |> toResult
                
let parse = 
    function
    | Lambda -> Interpreter.parse >> toResult
    | Oxalc  -> OxalcParser.parse  >> toResult


