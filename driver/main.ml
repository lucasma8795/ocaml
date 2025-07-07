let () =
  let run () = exit (Maindriver.main Sys.argv Format.err_formatter) in
  Handler_common.handle run
