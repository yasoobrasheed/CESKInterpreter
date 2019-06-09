structure Store :> sig

  type store

  val init     : store
  val malloc   : unit -> Address.adr   
  val lookup   : store * Address.adr -> Value.value
  val add      : store * Address.adr * Value.value -> store
  val replace  : store * Address.adr * Value.value -> store
  val tos      : store -> string
          
end = struct

  structure A = Address
  structure V = Value

  structure adrOrdKey : ORD_KEY = struct
    type ord_key = A.adr
    val compare  = A.compare
  end

  structure adrMap = ListMapFn(adrOrdKey)
  type store = Value.value adrMap.map
  
  fun unwrapInt (SOME n) = n
    | unwrapInt (NONE)   = raise Fail "Can't unwrap NONE"
  
  fun ++ r =
    let
      val curr = !r
      val newval = curr+1
    in
      (r:=newval; newval)
    end

  fun printMap [] = ""
    | printMap ((fst, scd)::[]) = (A.tos fst) ^ ":" ^ (V.tos scd)
    | printMap ((fst, scd)::ps) = (A.tos fst) ^ ":" ^ (V.tos scd) ^ ", " ^ (printMap ps)

  val i    = ref (unwrapInt Int.minInt)
  val init = adrMap.empty
  
  fun malloc _          = A.fromInt (++i)
  fun lookup (s, a)     = (case (adrMap.find (s, a)) of 
                            SOME v => v
                            | NONE => raise Fail "address is not in store") 
  fun add (s, a, v)     = adrMap.insert (s, a, v)
  fun replace (s, a, v) = add (s, a, v)
  fun tos s             = printMap (adrMap.listItemsi s)

end
