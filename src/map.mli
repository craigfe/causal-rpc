
type task = string * string
type job = string

type 'v contents =
  | Value of 'v
  | Task_queue of (task list * task list)
  | Job_queue of job list

module MakeContents (Val: Irmin.Contents.S) : Irmin.Contents.S
  with type t = Val.t contents

module type S = sig

  type key = string
  (** The type of the map keys *)

  type value
  (** The type of the map values *)

  type operation = string
  (** The type of operations to be performed on the map *)

  type t
  (** The type of maps from type [key] to type [value] *)

  module Contents: Irmin.Contents.S with type t = value contents
  module Store: Irmin.KV with type contents = Contents.t
  module Sync: Irmin.SYNC with type db = Store.t

  (* TODO: make this a more general type *)
  val of_store: Sync.db -> t
  (** Return the map corresponding to an underlying store representation *)

  val empty: ?directory:string -> unit -> t
  (** The empty map. *)

  val is_empty: t -> bool
  (** Test whether a map is empty or not. *)

  val mem: key -> t -> bool
  (** [mem x m] returns true iff [m] contains a binding for [x] *)

  val add: key -> value -> t -> t
  (** [add x y m] returns a map containing the same bindings as [m],
      plus a binding of [x] to [y]. If [x] was already bound in [m],
      its previous binding is replaced. *)

  val find: key -> t -> value
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val remove: key -> t -> t
  (** [remove x m] returns a map containing the same bindings as [m],
      except for [x] which is unbound in the returned map. *)

  val size: t -> int
  (** Return the number of bindings in the map *)

  val keys: t -> key list
  (** Return a list of keys in the map *)

  val values: t -> value list
  (** Return a list of values in the map *)

  val map: operation -> t -> t
  (** [map m] returns a map with the same domain as [m] in which
      the associated value [a] of all bindings of [m] have been
      replaced by the result of applying _a_ function to [a] *)
end

module Make (Val : Irmin.Contents.S) (Desc: Interface.DESC with type t = Val.t) : S
  with type value = Val.t
   and type operation = Interface.Description.op
(** Functor building an implementation of the map structure
    given an equality type and a set of operations on that type. *)
