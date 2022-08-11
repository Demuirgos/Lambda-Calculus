# Lambda-Calculus
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :
 
* in a separate file called library.oxalc :
```fsharp
let library := (reader) => 
    let ** : bool -> number -> number = (c:bool, n:number) => c ? n : 0 in (*if check is false it give 0*)
    let add-to-two := (n : number) => n + 2 in 
    reader
end library 
```
* in our main file :
```fsharp
let program := 
    include [library] for 
    let cst := x * f(x) where x : number = 3
                        and   f : number -> number = (n) => n - 2 in  
    let value := add-to-two(5) in 
    if value > 5 ? value = 11 : false then    
        [23, 2, 3] ** 3
    else    
       (23 * 3) + cst
end program 
```
