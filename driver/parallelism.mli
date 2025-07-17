module ThreadPool : sig
  type t

  val create : int -> t
  (** [create n] creates a thread pool with [n] new domains.
      Raises {!Invalid_argument} if [n] is not positive. *)

  val submit : t -> (unit -> unit) -> unit
  (** [submit pool task] submits a task to be executed by the thread pool.
      todo: do we want a (weakly) polymorphic return type 'a? *)

  val shutdown : t -> unit
  (** [shutdown pool] waits until all tasks are finished, then tears down
      the thread pool. *)
end
