structure Continuation = struct

	structure V = Value
	structure Env = Environment
	structure Exp = Expression
	
	datatype continuation 
		= Letk of V.value * Env.environment * Exp.exp * continuation
		| Halt

	fun tos (Letk (v, env, ex, c)) = "(let ([" ^ (V.tos v) ^ " " ^ (Exp.tos ex) ^ "]) " ^ (tos c) ^ ")" 
	  | tos (Halt) = "halt" 

end