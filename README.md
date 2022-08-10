# Lambda-Calculus (WIP)
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :
 
* in a separate file called library.oxalc :
```fsharp
let library := (reader) => 
    let ** : Atom<List> -> Atom<Number> -> Atom<Number> = (h::t, n) => h * n in (*gets the head and multiply it by n*)
    let add-to-two := (n : Atom) => n + 2 in 
    reader
end library 
```
* in our main file :
```fsharp
let program := 
    include [library] for 
    let cst := x * f(x) where x : Atom = 3
                        and f : Atom -> Atom = (n) => n - 2 in  
    let value : Atom = add-to-two(5) in 
    if value > 5 ? value = 11 : false then    
        [23, 2, 3] ** 3
    else    
       (23 * 3) + cst
end program 
```
