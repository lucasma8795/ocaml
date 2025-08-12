open Effect_handler

let () =
  let run () = exit (Custom_maindriver.main Sys.argv Format.err_formatter) in
  base_effect_handler run
