(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Lucas Ma, University of Cambridge                    *)
(*                                                                        *)
(*   Copyright 2025 Lucas Ma.                                             *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module provides various tools that helps abstract away the low-level
    primitives of parallelism found in stdlib.
*)

type !'a promise
(** Type of a promise, representing an asynchronous return value that will
    eventually be available. *)

val await : 'a promise -> 'a
(** [await p] blocks the calling domain until the promise [p] is resolved,
    returning the value if it was resolved, or re-raising the wrapped exception
    if it was rejected. *)

val try_resolve : 'a promise -> 'a option
(** [try_resolve p] returns [Some x] if it was resolved with value [x], or [None]
    if it has not been resolved. Re-raises the exception if it was rejected. *)

module TSQueue : sig
  type 'a t
  (** Type of a thread-safe queue. *)

  val create : unit -> 'a t
  (** [create ()] creates a new thread-safe queue. *)

  val add : 'a -> 'a t -> unit
  (** [add x q] adds the element [x] to the end of the queue [q]. *)

  val take : 'a t -> 'a
  (** [take q] blocks the calling domain until an element is available in the
      queue [q], then pops the first element and returns it. *)

  val take_opt : 'a t -> 'a option
  (** [take_opt q] pops the first element from the queue [q] if available,
      returning it as [Some x]. If the queue is empty, returns [None]. *)

  val is_empty : 'a t -> bool
  (** [is_empty q] returns [true] if the queue [q] is empty, otherwise
      [false]. May cause race conditions, use with caution! *)
end

module Pool : sig
  type t
  (** Type of a thread pool. *)

  val create : int -> t
  (** [create n] creates a thread pool with [n] new domains.
      Raises {!Invalid_argument} if [n] is not positive. *)

  val submit : t -> (unit -> 'a) -> 'a promise
  (** [submit pool task] submits a task to be executed by the thread pool. *)

  val join_and_shutdown : t -> unit
  (** [join_and_shutdown pool] waits until all tasks are finished, then tears
      down the thread pool. *)
end

val suspend_on : unit promise -> unit
(** [suspend p] suspends the calling task until the promise [p] is resolved,
    allowing other tasks to run on the calling domain.
    Must be called from within a task that is submitted to a pool, otherwise
    effects may not be handled.

    todo: make polymorphic? *)
