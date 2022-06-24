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
                    | _ -> fun () -> "let program := " + Console.ReadLine() + " end program" 
        let eval mode = 
            let operation = match mode with 
                            | Lambda -> Interpreter.parse
                            | _ ->  transpile LCR
            operation >> function 
                | Success(code,_) -> code |> (interpret >> toString)
                | error-> error |> toResult
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
    | [||]         -> execute <| Terminal
    | [|"-lambda"|]-> execute <| Lambda
    | [|"-path";p|]-> execute <| File(p)
    | _            -> failwith "Usage: Can Only Run 1 File at a time"