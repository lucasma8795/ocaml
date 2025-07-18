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

type !'a promise
(** Type of a promise, representing a asynchronous return value that will
    eventually be available. *)

val await : 'a promise -> 'a
(** [await p] blocks the current thread until the promise [p] is resolved,
    returning the value if it was resolved, or re-raising the wrapped exception
    if it was rejected. *)

module ThreadPool : sig
  type t
  (** Type of a thread pool. *)

  val create : int -> t
  (** [create n] creates a thread pool with [n] new domains.
      Raises {!Invalid_argument} if [n] is not positive. *)

  val submit : 'a. t -> (unit -> 'a) -> 'a promise
  (** [submit pool task] submits a task to be executed by the thread pool. *)

  val shutdown : t -> unit
  (** [shutdown pool] waits until all tasks are finished, then tears down
      the thread pool. *)
end
