structure Expression = struct

	datatype prim
		= Add
		| Minus
		| Times
		| Equals

	datatype aexp
		= Nat of int
		| True
		| False
		| Var of string
		| Lambda of aexp list * exp
		| Prim of prim * aexp list

	datatype cexp
		= Sequence of aexp list
		| If of aexp * exp * exp
		| Call of aexp
		| Set of string * aexp
    	| Record of (string * aexp) list

	datatype exp
		= AExp aexp
		| CExp cexp
		| Let (aexp * exp) * exp
	
	fun isAtom (AExp a) = (case a of 
							(Nat n) => true
							| (True) => true
							| (False) => true
							| (Var s) => true
							| (Lambda ([], e)) => true
							| (Lambda ((al::als), e)) => (case al of
															(Var s) = isAtom (AExp (Lambda (als, e)))
															| _		= false)
							| (Prim (p, [])) => true
							| (Prim (p, (a::als))) => isAtom (AExp a) andalso isAtom (AExp (Prim (p, als))))
	  | isAtom _ = false

	fun tosHelper (tosF, []) = ""
	  | tosHelper (tosF, (t::ts)) = (tosF t) ^ tosHelper (tosF, ts)  

	fun ptos (Add) = "+"
	  | ptos (Minus) = "-"
	  | ptos (Times) = "*"
	  | ptos (Equals) = "="

	fun atos (Nat n) = Int.toString n
	  | atos (True) = "true"
	  | atos (False) = "false"
	  | atos (Var s) = s
	  | atos (Lambda (als, e)) "(' " ^ (tosHelper (atos, als)) ^ " " ^ (tos e) ^ ")"
	  | atos (Prim (p, als)) = (ptos p) ^ " (" ^ (tosHelper (atos, als)) ^ ")" 

	fun ctos (Sequence als) = "(" ^ (tosHelper (atos, als)) ^ ")"
	  | ctos (If (a, e1, e2)) = "(if " ^ (atos a) ^ " " ^ (tos e1) ^ " " ^ (tos e2) ^ ")"
	  | ctos (Call a) = "(call " ^ (atos a) ^ ")"
	  | ctos (Set (s, a)) = "(set! " ^ s ^ " " ^ (atos a) ^ ")"
      | ctos (Record ps) = "(" ^ (String.concatWith " " (map (fn (lab, a) => lab ^ " " ^ atos a) ps)) ^ ")"

	fun tos (AExp a) = atos a
	  | tos (CExp c) = ctos c
	  | tos (Let ((a, e1), e2)) = "(let ((" ^ (atos a) ^ " " ^ (tos e1) ^ ")) " ^ (tos e2) ^ ")"
end
