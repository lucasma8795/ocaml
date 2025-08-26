open Clflags
open Parallelism

module Options = Main_args.Make_bytecomp_options (Main_args.Default.Main)

let with_info ~dump_ext unit_info k =
  (* Compmisc.init_path (); *)
  Env.reset_cache ();
  Env.set_current_unit unit_info ;
  let env = Compmisc.initial_env () in
  let dump_file = String.concat "." [Unit_info.prefix unit_info; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k Compile_common.{
    target = unit_info;
    env;
    ppf_dump;
    tool_name = "ocamlc";
    native = false;
  }

let implementation ~start_from ~source_file ~output_prefix =
  let backend info typed =
    let bytecode = Compile.to_bytecode info typed in
    Compile.emit_bytecode info bytecode
  in
  let unit_info = Unit_info.make ~source_file Impl output_prefix in
  with_info ~dump_ext:"cmo" unit_info @@ fun info ->
  match (start_from : Clflags.Compiler_pass.t) with
  | Parsing -> Compile_common.implementation info ~backend
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)

let interface ~source_file ~output_prefix =
  let unit_info = Unit_info.make ~source_file Intf output_prefix in
  with_info ~dump_ext:"cmi" unit_info @@ fun info ->
  Compile_common.interface info

let process_deferred_actions ctx =
  let final_output_name = !Clflags.output_name in
  (* Make sure the intermediate products don't clash with the final one
     when we're invoked like: ocamlopt -o foo bar.c baz.ml. *)
  if not !compile_only then Clflags.output_name := None;
  begin
    match final_output_name with
    | None -> ()
    | Some _output_name ->
        if !compile_only then begin
          if List.length (List.filter (function
              | Compenv.ProcessCFile _
              | Compenv.ProcessImplementation _
              | Compenv.ProcessInterface _ -> true
              | _ -> false) !Compenv.deferred_actions) > 1 then
            Compenv.fatal "Options -c -o are incompatible with compiling multiple files"
        end;
  end;

  if !make_archive then begin
    if List.exists (function
        | Compenv.ProcessOtherFile name -> Filename.check_suffix name ".cmxa"
        | _ -> false) !Compenv.deferred_actions then
      Compenv.fatal "Option -a cannot be used with .cmxa input files."
    end
  else if !Compenv.deferred_actions = [] then
    Compenv.fatal "No input files";

  (* start all compilations *)
  let files_to_compile = List.filter_map (function
    | Compenv.ProcessImplementation ml_file -> Some ml_file

    | Compenv.ProcessInterface name ->
        raise (Arg.Bad(Printf.sprintf "[process_deferred_actions] don't know what to do with ProcessInterface (name=%s)\n%!" name))

    | Compenv.ProcessCFile name ->
        raise (Arg.Bad(Printf.sprintf "[process_deferred_actions] don't know what to do with ProcessCFile (name=%s)\n%!" name))

    | Compenv.ProcessObjects names ->
        raise (Arg.Bad(Printf.sprintf "[process_deferred_actions] don't know what to do with ProcessObjects (names=%s)\n%!" (String.concat ", " names)))

    | Compenv.ProcessDLLs names ->
        raise (Arg.Bad(Printf.sprintf "[process_deferred_actions] don't know what to do with ProcessDLLs (names=%s)\n%!" (String.concat ", " names)))

    | Compenv.ProcessOtherFile name ->
        raise (Arg.Bad(Printf.sprintf "[process_deferred_actions] don't know what to do with ProcessOtherFile (name=%s)\n%!" name))

  ) (List.rev !Compenv.deferred_actions)
  in
  compile_ml_files ctx files_to_compile;

  Printf.eprintf "[process_deferred_actions] all done! exiting...\n%!";

  Clflags.output_name := final_output_name;
  Compenv.stop_early :=
    !compile_only ||
    !print_types ||
    match !stop_after with
    | None -> false
    | Some p -> Clflags.Compiler_pass.is_compilation_pass p

let main argv ppf =
  let program = "ocamlc" in
  Clflags.add_arguments __LOC__ Options.list;
  Clflags.add_arguments __LOC__
    ["-depend", Arg.Unit Makedepend.main_from_option,
     "<options> Compute dependencies (use 'ocamlc -depend -help' for details)"];
  let exception Continue in
  match
    Compenv.readenv ppf Before_args;
    Compenv.parse_arguments (ref argv) Compenv.anonymous program;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    begin try
      process_deferred_actions Compenv.{
        log = ppf;
        compile_implementation = implementation;
        compile_interface = interface;
        ocaml_mod_ext = ".cmo";
        ocaml_lib_ext = ".cma";
      }
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments program;
        exit 2
      end
    end;
    if Clflags.(should_stop_after Compiler_pass.Lambda)
      then raise Continue;
    Compenv.readenv ppf Before_link;
    if
      List.length
        (List.filter (fun x -> !x)
           [make_archive;make_package;Compenv.stop_early;output_c_object])
        > 1
    then begin
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None ->
          Compenv.fatal
            "Please specify at most one of -pack, -a, -c, -output-obj";
      | Some ((P.Parsing | P.Typing | P.Lambda) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf Compenv.fatal
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -output-obj"
          (String.concat "|"
             (P.available_pass_names ~filter:(fun _ -> true) ~native:false))
      | Some (P.Scheduling | P.Emit) -> assert false (* native only *)
    end;
    if !make_archive then begin
      Compmisc.init_path ();

      Bytelibrarian.create_archive
        (Compenv.get_objfiles ~with_ocamlparam:false)
        (Compenv.extract_output !output_name);
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path ();
      let extracted_output = Compenv.extract_output !output_name in
      let revd = Compenv.get_objfiles ~with_ocamlparam:false in
      Compmisc.with_ppf_dump ~file_prefix:extracted_output (fun ppf_dump ->
        Bytepackager.package_files ~ppf_dump (Compmisc.initial_env ())
          revd (extracted_output));
      Warnings.check_fatal ();
    end
    else if not !Compenv.stop_early && !objfiles <> [] then begin
      let target =
        if !output_c_object && not !output_complete_executable then
          let s = Compenv.extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll
            || Filename.check_suffix s ".c")
          then s
          else
            Compenv.fatal
              (Printf.sprintf
                 "The extension of the output file must be .c, %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          Compenv.default_output !output_name
      in
      Compmisc.init_path ();
      Bytelink.link (Compenv.get_objfiles ~with_ocamlparam:true) target;
      Warnings.check_fatal ();
    end;
  with
  | exception (Compenv.Exit_with_status n) ->
    n
  | exception Continue
  | () ->
    Compmisc.with_ppf_dump ~file_prefix:"profile"
      (fun ppf -> Profile.print ppf !Clflags.profile_columns);
    0
  | exception x ->
  Location.report_exception ppf x;
  2

(* CAML_LD_LIBRARY_PATH= ../v1/bin/ocamlc ocamlcommon.cma ocamlbytecomp.cma unix.cma custom_ocamlc.ml -I +compiler-libs -I +unix -o custom-ocamlc *)
