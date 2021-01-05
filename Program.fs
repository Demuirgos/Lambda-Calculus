open System
open Parsec
open Interpreter

[<EntryPoint>]
let main (args:string []) =
    let word = "Lake" 
    (run (Seq.toList word)  (expect 'L') ) |> printf "%A"
    0