# LambdaCalculus
	a simple interpreter of Lambdas using F# parsec
# LambdaCalculus Language Grammar :
	 Expression := 
		(Expression Expression)
		| (lambda arg. Expression)
		| Name
	Name       := [a-z]+
