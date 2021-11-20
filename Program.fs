open System
open System.IO
open FSharp.Core
open Interpreter
open Abstractor
type Mode = File of string | Terminal | Lambda
[<EntryPoint>]
let REPL args =
    let execute mode= 
        Console.OutputEncoding <- Text.Encoding.Unicode
        let prefix = "λ >"
        let read =  function
                    | File(path) -> fun () -> [yield! File.ReadLines(path)]
                                              |> String.concat "\n"
                    | _ -> Console.ReadLine 
        let eval mode = 
            match mode with 
            | Lambda -> interpret
            | _ -> transpile >> interpret
        let print= 
            function 
            | input ->  printfn "%s" input
                        prefix      
        let rec loop state prefix =
            prefix |> printf "%s "
            |> read(state) |> eval(state)
            |> print |> loop (match state with 
                              | Lambda -> Lambda
                              | _ -> Terminal )
        loop mode prefix
    match args with 
    | [||]       -> execute <| Terminal
    | [|path|]   -> execute <| File(path)
    | [|"-lmbd"
        "true"|] -> execute <| Lambda
    | _          -> failwith "Usage: Can Only Run 1 File at a time"


