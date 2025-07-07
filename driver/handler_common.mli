(* common interface for main and optmain to catch effects *)
val handle : (unit -> 'a) -> 'a
