open System
open Interpreter
open Abstractor

[<EntryPoint>]
let REPL _ =
    Console.OutputEncoding <- Text.Encoding.Unicode
    let prefix = "λ >"
    let read = Console.ReadLine >> sprintf "%s"
    let eval = transpile >> interpret
    let print= 
        function 
        | input ->  printfn "%s" input
                    prefix      
    let rec loop prefix = 
        prefix |> printf "%s"
        |> read |> eval |> print |> loop
    loop prefix

