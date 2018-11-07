module type EqualityType = sig
  type t
  val (=) : t -> t -> bool
  val of_string: string -> t
  val to_string: t -> string

end
(** Input signature of the functor {!Set.Make}. *)

module type Operations = sig
  type t

  (** HACK: Just change the value in some way; used for testing the map
      operator on sets while we don't have a functor implementation *)
  val iter: t -> t
end

module type S = sig
  type key = string
  (** The type of the map keys *)

  type value
  (** The type of the map values *)

  type t
  (** The type of maps from type [key] to type [value] *)

  val empty: ?directory:string -> unit -> t
  (** The empty map. *)

  val of_store: Irmin_unix.Git.FS.KV(Irmin.Contents.String).t -> t
  (* TODO: change this to take a more general type *)
  (** Return the map corresponding to an underlying store representation *)

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

  (* TODO: generalise this to one of a set of functions *)
  val map: t -> t
  (** [map m] returns a map with the same domain as [m] in which
      the associated value [a] of all bindings of [m] have been
      replaced by the result of applying _a_ function to [a] *)
end

module Make (Eq : EqualityType) (Op: Operations with type t = Eq.t) : S
  with type value = Eq.t
(** Functor building an implementation of the map structure
    given an equality type and a set of operations on that type. *)
