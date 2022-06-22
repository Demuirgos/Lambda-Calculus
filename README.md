# Lambda-Calculus
A simple Programming Language Compiler to Lambda-Calculus, with a Lambda-Runtime 
Oxalc Example :
 
* in a separate file called library.oxalc :
```fsharp
let library := 
    let ** := (h::t, n) => h * n in (*this function gets the head and multiplies it by n*)
    let add-to-two := (n) => n + 2 in 
    ???
end library 
```

* in our main file :
```fsharp
let program := 
    include [library] for 
    let cst := x * f(x) where x := 3
                        and f := (n) => n - 2 in  
    let value := add-to-two(5) in 
    if value > 5 ? value = 11 : false then    
        [23, 2, 3] ** 3
    else    
       (23 * 3) + cst
end program 
```
