# Lambda-Calculus
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example : 
```fsharp
let program := 
    let cst := x * f(x) where x := 3
                        and f := (n) => n - 2 in
    let mult-by-n := (h::t, n) => h * m in
    let add-to-two := (n) => n + 2 in 
    let value := add-to-two(5) in 
    if value > 5 ? value = 10 : false then    
        mult-by-n([23, 2, 3], 3)
    else    
       (23 * 3) + cst
end program 
```
