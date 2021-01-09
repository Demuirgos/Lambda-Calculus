open System
open Parsec
open Interpreter

[<EntryPoint>]
let main (args:string []) =
    Console.OutputEncoding <- Text.Encoding.Unicode
    let word = "(-35.3)" 
    // let pnumber =
        // let testp p = between (expect '(') p (expect ')') 
        // let pint = "0123456789" |> Seq.toList |> anyOf |> many 1 
        // let psign = option (expect '-')
        // let psep = expect '.'
        // let pnumber = psign .>>. pint .>>. option (psep .>>. pint)
        // let mapping = 
            // let arr2Str chars = chars |> (List.map string) |> List.reduce (+)
            // function
            // | ((Some sign,part1),Some (point,part2)) -> 
                // sprintf "-%s%c%s" (arr2Str part1) (point) (arr2Str part2) |> float
            // | ((Some sign,part1),None) -> 
                // sprintf "-%s" (arr2Str part1) |> float
            // | ((None,part1),Some (point,part2)) -> 
                // sprintf "%s%c%s" (arr2Str part1) (point) (arr2Str part2) |> float
            // | ((None,part1),None) -> 
                // sprintf "%s" (arr2Str part1) |> float
        // testp pnumber <?> "test"
        // |> map mapping
    // (word |> fromStr,pnumber) ||> run  |> toResult |> printf "%A\n"
    let test =(fromStr "(λx.x t)", parseExpr .>> eof) 
            ||> run 
             |> getContent 
             |> evalExpr 
    test |> toString |> printfn "%s"
    0
