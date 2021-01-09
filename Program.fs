open System
open Parsec
open Interpreter

[<EntryPoint>]
let main (args:string []) =
<<<<<<< Updated upstream
    // let word = "(-35.3)" 
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
    (fromStr "λx.y", parseExpression) 
    ||> run
     |> toResult
     |> printf "%A"
=======
<<<<<<< Updated upstream
    let word = "-35,3" 
    // let verbParser = Parser {
    //     let subject = Parser {
    //         let proc = fun w -> w |> (Seq.toList >> allOf)
    //         return (proc "the") .>>. expect ' ' .>>. (proc "lake") .>>. expect ' '
    //     }
    //     let verb = expect 'i' .>>. expect 's'
    //     let object = (expect ' ') .>>. (allOf <| Seq.toList "dry" )
    //     return between subject verb object
    // }
    //((Seq.toList word),verbParser) ||> run  |> printf "%A"
    let pint = 
        let intp = "0123456789" |> Seq.toList |> anyOf |> many 1 
        let signp = option (expect '-')
        let sepp = expect '.'
        let pnumber = intp .>>. option (sepp .>>. intp)
        let mapping (((sign,part1),point),part2) = 
            let arr2Str chars = chars |> (List.map string) |> List.reduce (+) 
            let absValue = sprintf "%s%c%s" (arr2Str part1) (point) (arr2Str part2) |> float
            match sign with 
            | Some _ -> - absValue
            | _ -> absValue
        (signp .>>. intp .>>. sepp .>>. intp) <?> "Number"
        |> map mapping
    (word |> fromStr,pint) ||> run  |> toResult |> printf "%A"
    ("ab" |> fromStr) />? (expect 'a') |> printf "%A"
=======
    Console.OutputEncoding <- Text.Encoding.Unicode
    let word = "(-35.3)" 
    let pnumber =
        let testp p = between (expect '(') p (expect ')') 
        let pint = "0123456789" |> Seq.toList |> anyOf |> many 1 
        let psign = option (expect '-')
        let psep = expect '.'
        let pnumber = psign .>>. pint .>>. option (psep .>>. pint)
        let mapping = 
            let arr2Str chars = chars |> (List.map string) |> List.reduce (+)
            function
            | ((Some sign,part1),Some (point,part2)) -> 
                sprintf "-%s%c%s" (arr2Str part1) (point) (arr2Str part2) |> float
            | ((Some sign,part1),None) -> 
                sprintf "-%s" (arr2Str part1) |> float
            | ((None,part1),Some (point,part2)) -> 
                sprintf "%s%c%s" (arr2Str part1) (point) (arr2Str part2) |> float
            | ((None,part1),None) -> 
                sprintf "%s" (arr2Str part1) |> float
        testp pnumber <?> "test"
        |> map mapping
    (word |> fromStr,pnumber) ||> run  |> toResult |> printf "%A\n"
    (fromStr "(λx.y z)", parseExpression .>> eof) 
    ||> run
    |> toResult
    |> printf "%s"
>>>>>>> Stashed changes
>>>>>>> Stashed changes
    0
