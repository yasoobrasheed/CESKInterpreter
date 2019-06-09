structure Value = struct
	
	structure Continuation = K
	structure Closure = C
	structure Expression = E

	datatype value
		= Void
		| Cont of K.continuation
		| Exp of E.exp
		| Clos of C.closure

	fun isVal (Void) = true
	  | isVal (Clos c) = C.isClos c
	  | isVal (Cont k) = true
	  | isVal (Exp e)  = if E.isAtom e then (case e of 
	  											(E.True) => true
	  											| (E.False) => true
	  											| (E.Nat n) => true
	  											| _			=> false)
	  						else false

	fun tos (Void) = "void"
	  | tos (Cont k) = K.tos k 
	  | tos (Clos c) = C.tos c
	  | tos (Exp e)	 = E.tos e

end