open System
open Interpreter
open Abstractor

let REPL _ =
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

[<EntryPoint>]
let main _ =
    let rec loop () = 
        let program = Console.ReadLine() |> sprintf "%s"
        let lambdas = transpile program 
        let results = interpret lambdas    
        printf "%A\n" (program, lambdas, results)              
        |> loop
    loop ()
    0
