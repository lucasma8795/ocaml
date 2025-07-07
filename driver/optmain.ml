let () =
  let run () = exit (Optmaindriver.main Sys.argv Format.err_formatter) in
  Handler_common.handle run
