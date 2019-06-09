structure Address :> sig

  eqtype adr

  val fromInt : int -> adr
  val compare : adr * adr -> order
  val equal   : adr * adr -> bool
  val tos     : adr -> string
			    
end = struct

  type adr = int

  fun fromInt x = x

  fun compare (l1, l2) = Int.compare (l1, l2)

  fun equal (l1 : int, l2 : int) = l1 = l2

  fun tos l = Int.toString l 
		      
end