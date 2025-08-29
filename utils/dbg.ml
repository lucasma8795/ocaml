let dbg_lock = Mutex.create ()

let dbg_enabled = false

let dbg fmt =
  if dbg_enabled then
    Mutex.protect dbg_lock (fun () ->
      let ret = Printf.eprintf ("%d -> " ^^ fmt) (Domain.self () :> int) in
      flush stderr;
      ret
    )
  else
    Printf.ifprintf stderr fmt

let dbg_print_backtrace () =
  Mutex.protect dbg_lock (fun () ->
    Printexc.print_backtrace stderr;
    flush stderr
  )
