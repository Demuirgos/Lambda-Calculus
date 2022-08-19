open System
open System.IO
open FSharp.Core
open Typedefinitions
open Interpreter
open OxalcParser
open OxalcCompiler

type Mode = File of string | Terminal | Lambda
[<EntryPoint>]
let REPL args =
    let execute mode backend= 
        Console.OutputEncoding <- Text.Encoding.Unicode
        let prefix = "λ >"
        let read =  function
                    | File(path) -> fun () -> [yield! File.ReadLines(path)]
                                              |> String.concat "\n"
                    | _ -> fun () -> "let program := include [] for " + Console.ReadLine() + " end program" 
        let eval mode = 
            let operation = match mode with 
                            | Lambda -> Interpreter.parse
                            | _ ->  transpile backend
            operation >> function 
                | Ok (codeResult,_) -> 
                    match codeResult with 
                    | LCRR(code) -> code |> (interpret >> toString)
                    | JSR(code) -> code 
                    | _ -> "Invalid program"
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
    | [||]         -> execute Terminal LCR
    | [|"--mode"; "lambda"|]-> execute Lambda LCR
    | [|"--path"; path; "--backend"; target|]-> execute (File(path)) (Backend.Parse target)
    | _            -> failwith "Usage: Can Only Run 1 File at a time"