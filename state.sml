structure State = struct

	structure Env = Environment
	structure C = Expression
	structure K = Continuation
	structure S = Store

	datatype state 
		= State of C.exp * Env.environment * S.store * K.continuation

end