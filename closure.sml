structure Closure = struct
	
	datatype closure 
		= Closure of Expression.exp * Environment.environment

	fun isClos (Closure (exp, env)) = if (Expression.isAtom exp) then (case exp of
																	(Expression.AExp (Expression.Lambda _)) => true
																	| _ 		 => false)
							else false

	fun tos (exp, env) = (Expression.tos exp) ^ (Environment.tos env) 

end