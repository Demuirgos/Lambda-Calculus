# OxaLC
	a basic ML like language with a Lambda-Calculus based Compiler
	
# LambdaCalculus Language Grammar :
	 Terminology := 
		| Application : "(λy.z x)" = (Lambda Terminology)
		| Lambda      : "λx.y"     = λName.Terminology 
		| Name        : "x"        = [a-z]+
		
# MLAbstraction Language Grammar :
	 Expression := 
		| Bind(Name, Expression)   : "let x = y in x" 
		| Func([Name], Expression) : "let f = (y,w) => w in f"
		| Oper(Func, [Expression]) : "let x = f(y,z) in x"
		| Identifier 		   : [a-z]+
