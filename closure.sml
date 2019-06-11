structure Closure = struct
	
	structure Env = Environment
	structure Exp = Expression
	datatype closure 
		= Closure of Exp.exp * Env.environment

	fun isClos (Closure (exp, env)) = if (Exp.isAtom exp) then (case exp of
																	(Exp.Lambda _) => true
																	| _ 		   => false)
							else false

	fun tos (Closure (exp, env)) = (Exp.tos exp) ^ (Env.tos env) 

end