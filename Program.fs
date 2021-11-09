open System
open System.IO
open FSharp.Core
open Interpreter
open Abstractor
type Mode = File of string | Terminal
[<EntryPoint>]
let REPL args =
    let execute mode= 
        Console.OutputEncoding <- Text.Encoding.Unicode
        let prefix = "λ >"
        let read =  function
                    | Terminal   -> Console.ReadLine >> sprintf "%s"
                    | File(path) -> fun () -> [yield! File.ReadLines(path)]
                                              |> String.concat "\n"
        let eval = transpile >> interpret
        let print= 
            function 
            | input ->  printfn "%s" input
                        prefix      
        let rec loop state prefix =
            prefix |> printf "%s "
            |> read(state) |> eval 
            |> print |> loop Terminal
        loop mode prefix
    match args with 
    | [||]     -> execute <| Terminal
    | [|path|] -> execute <| File(path)
    | _        -> failwith "Usage: Can Only Run 1 File at a time"


