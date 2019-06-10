structure Expression = struct

	structure P = Prim

	datatype exp
		= Nat of int
		| True
		| False
		| Var of string
		| Lambda of exp list * exp
		| Pr of P.Prim * exp list
		| Sequence of exp list
		| If of exp * exp * exp
		| Call of exp
		| Set of string * exp
		| Record of (string * exp) list
		| Let of exp * exp * exp
	
	fun isAtom (Nat n) = true
	  | isAtom (True) = true
	  | isAtom (False) = true
	  | isAtom (Var s) = true
	  | isAtom (Lambda ([], e)) = true
	  | isAtom (Lambda ((al::als), e)) = (case al of
											(Var s) => isAtom (Lambda (als, e))
											| _		=> false)
	  | isAtom (Pr (p, [])) = true
	  | isAtom (Pr (p, (a::als))) = isAtom a andalso isAtom (Pr (p, als))
	  | isAtom _ = false

	fun isCom (Sequence []) = true
	  | isCom (Sequence (a::als)) = isAtom a andalso isCom (Sequence als) 
	  | isCom (If (a1, e2, e3)) = isAtom a1
	  | isCom (Call a) = isAtom a
	  | isCom (Set (s, a)) = isAtom a
	  | isCom (Record []) = true
	  | isCom (Record ((s, a)::ps)) = isAtom a andalso isCom (Record ps) 
	  | isCom _ = false

	fun isExp (Let (e1, e2, e3)) = (case e1 of 
										(Var s) => true
										| _     => false) 
	 | isExp e = isAtom e orelse isCom e  

	fun tosHelper (tosF, []) = ""
	  | tosHelper (tosF, (t::ts)) = (tosF t) ^ tosHelper (tosF, ts)  

	fun tos (Nat n) = Int.toString n
	  | tos (True)  = "true"
	  | tos (False) = "false"
	  | tos (Var s) = s
	  | tos (Lambda (als, e)) = "(' " ^ (tosHelper (tos, als)) ^ " " ^ (tos e) ^ ")"
	  | tos (Pr (p, als)) = (P.tos p) ^ " (" ^ (tosHelper (tos, als)) ^ ")" 
	  | tos (Sequence als) = "(" ^ (tosHelper (tos, als)) ^ ")"
	  | tos (If (a, e1, e2)) = "(if " ^ (tos a) ^ " " ^ (tos e1) ^ " " ^ (tos e2) ^ ")"
	  | tos (Call a) = "(call " ^ (tos a) ^ ")"
	  | tos (Set (s, a)) = "(set! " ^ s ^ " " ^ (tos a) ^ ")"
      | tos (Record ps) = "(" ^ (String.concatWith " " (map (fn (lab, a) => lab ^ " " ^ tos a) ps)) ^ ")"
	  | tos (Let (a, e1, e2)) = "(let ((" ^ (tos a) ^ " " ^ (tos e1) ^ ")) " ^ (tos e2) ^ ")"

end