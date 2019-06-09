structure Closure = struct

	structure Expression = Exp
	structure Environment = Env
	datatype closure = Exp.exp * Env.environment

	fun isClos (exp, env) = if (Exp.isAtom exp) then (case exp of
														(Exp.AExp (Exp.Lambda _)) => true
														| _ 		 => false)
							else false

	fun tos (exp, env) = (Exp.tos exp) ^ (Env.tos env) 

end