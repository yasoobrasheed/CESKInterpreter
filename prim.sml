structure Prim = struct

	datatype prim
		= Add
		| Minus
		| Times
		| Equals

	fun tos (Add) = "+"
	  | tos (Minus) = "-"
	  | tos (Times) = "*"
	  | tos (Equals) = "="

end