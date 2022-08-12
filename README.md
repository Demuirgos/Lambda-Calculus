# Lambda-Calculus (WIP)
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :
 
* in a separate file called library.oxalc :
```fsharp
let library := { 
        **    := (c:bool, n:number) => c ? n : 0; (*if check is false it give 0*)
        incr  := (n : number) => n + 1;
        zero  : number = 0;
        boolF : bool = false 
    }
end library  
```
* in our main file :
```fsharp
let program := 
    include [library] for 
    let value := incr(0) in 
    if value = 1 ? value < 2 : false then    
        (value = 2) ** 1
    else    
        (1 * 2) + value
end program 
```
this program yields the following result : 
```
val it : number = λ_a.λ_b._b
```
