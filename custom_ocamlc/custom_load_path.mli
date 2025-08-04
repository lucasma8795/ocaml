open Load_path

val visible_dirs : Dir.t list ref
val hidden_dirs : Dir.t list ref

val reset : unit -> unit
(** Remove all directories *)

val init : visible:string list -> hidden:string list -> unit
(** [init ~visible ~hidden] is the same as
    [reset ();
     List.iter add_dir (List.rev hidden);
     List.iter add_dir (List.rev visible)] *)

val auto_include_otherlibs :
  (string -> unit) -> auto_include_callback
(** [auto_include_otherlibs alert] is a callback function to be passed to
    {!Load_path.init} and automatically adds [-I +lib] to the load path after
    calling [alert lib]. *)

val get_path_list : unit -> string list
(** Return the list of directories passed to [add_dir] so far. *)

val find : string -> string
(** Locate a file in the load path. Raise [Not_found] if the file
    cannot be found. This function is optimized for the case where the
    filename is a basename, i.e. doesn't contain a directory
    separator. *)

val find_normalized : string -> string
(** Same as [find], but search also for normalized unit name (see
    {!Misc.normalized_unit_filename}), i.e. if name is [Foo.ml], allow
    [/path/Foo.ml] and [/path/foo.ml] to match. *)

val find_normalized_with_visibility : string -> string * Load_path.visibility
(** Same as [find_normalized], but also reports whether the cmi was found in a
    -I directory (Visible) or a -H directory (Hidden) *)

val append_dir : Dir.t -> unit
(** [append_dir d] adds [d] to the end of the load path (i.e. at lowest
    priority. *)

val add_new_file_to_path : string -> string -> unit

val prepend_add : Dir.t -> unit
