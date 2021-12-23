module OxAlc.Service.Library

open System
open System.IO
open FSharp.Core
open Interpreter
open Abstractor

type Mode = Lambda | Oxalc

let transpile = Abstractor.transpile >> sprintf "%A"

let interpret = Interpreter.parse 
                >> function
                | Success (code, _) -> code |> Interpreter.interpret |> sprintf "%A"
                | _ as error -> error |> toResult
                
let decompile = Interpreter.parse 
                >> function
                | Success (code, _) -> code |> Abstractor.uncompile |> (sprintf "%A")
                | _ as error -> error |> toResult
                
let parse = 
    function
    | Lambda -> Interpreter.parse >> toResult
    | Oxalc  -> Abstractor.parse  >> toResult


