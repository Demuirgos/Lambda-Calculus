module OxAlc.Service.Library

open System
open System.IO
open FSharp.Core
open Interpreter
open Abstractor

type Mode = Lambda | Oxalc

let Transpile = Interpreter.interpret >> sprintf "%A"
let Interpret = Abstractor.transpile >> sprintf "%A"
let Decompile = Interpreter.parse 
                >> function
                | Success (code, _) -> code |> Abstractor.uncompile |> (sprintf "%A")
                | _ as error -> error |> toResult
let Parse = 
    function
    | Lambda -> Interpreter.parse >> toResult
    | Oxalc  -> Abstractor.parse  >> toResult


