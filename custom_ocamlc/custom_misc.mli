val dbg : ('a, out_channel, unit) format -> 'a

val dbg_print_backtrace : unit -> unit

val compile_implementation : Compenv.action_context -> string -> unit

val compile_interface : Compenv.action_context -> string -> unit
