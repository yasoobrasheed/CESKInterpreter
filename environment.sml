structure Environment :> sig

  type environment

  val empty  : environment

  val extend : environment -> (string * Address.adr) -> environment

  val lookup : environment -> string -> Address.adr option

  val tos    : environment -> string
            
end = struct

  structure StringOrdKey : ORD_KEY = struct
      type ord_key = string
      val compare = String.compare
  end

  structure StringMap = ListMapFn(StringOrdKey)
  type environment = Address.adr StringMap.map

  fun printMap [] = ""
    | printMap ((fst, scd)::[]) = fst ^ ":" ^ (Address.tos scd)
    | printMap ((fst, scd)::ps) = fst ^ ":" ^ (Address.tos scd) ^ " " ^ (printMap ps)

  val empty = StringMap.empty
    
  fun lookup e s = StringMap.find (e, s)

  fun extend e (s, a) = StringMap.insert (e, s, a) 

  fun tos e = printMap (StringMap.listItemsi e)
          
end