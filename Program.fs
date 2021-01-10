open System
open Interpreter

[<EntryPoint>]
let REPL  =
    Console.OutputEncoding <- Text.Encoding.Unicode
    let prefix = "λ >"
    let read = Console.ReadLine >> sprintf "%s"
    let eval = interpret
    let print= 
        function 
        | input ->  printfn "%s" input
                    prefix      
    let rec loop prefix = 
        prefix |> printf "%s"
        |> read |> eval |> print |> loop
    loop prefix
