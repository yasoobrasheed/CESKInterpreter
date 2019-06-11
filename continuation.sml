structure Continuation = struct

	structure Env = Environment
	structure Exp = Expression
	
	datatype continuation 
		= Letk of Exp.exp * Env.environment * Exp.exp * continuation
		| Halt

	fun isCont (Halt) = true
	  | isCont (Letk (v, env, ex, k)) = (case v of 
	  										(Exp.Var s) => isCont k
	  										| _ 		=> false)

	val empty = Halt

	fun tos (Letk (v, env, ex, k)) = "(let ([" ^ (Exp.tos v) ^ " " ^ (Exp.tos ex) ^ "]) " ^ (tos k) ^ ")" 
	  | tos (Halt) = "halt" 

end