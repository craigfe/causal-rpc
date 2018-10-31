module type EqualityType = sig
  type t
  val (=) : t -> t -> bool
  val of_string: string -> t
  val to_string: t -> string
end
(** Input signature of the functor {!Set.Make}. *)

module type S = sig
  type elt
  (** The type of the set elements. *)

  type t
  (** The type of sets. *)

  val empty: ?directory: string -> unit -> t
  (** The empty set. *)

  val is_empty: t -> bool
  (** Test whether a set is empty or not. *)

  val mem: elt -> t -> bool
  (** [mem x s] tests whether [x] belongs to the set [s]. *)

  val add: elt -> t -> t
  (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

  val remove: elt -> t -> t
  (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)

  val size: t -> int
  (** Return the number of elements of a set. *)

  val elements: t -> elt list
  (** Return the list of all elements of the given set. *)
end

module Make (Eq : EqualityType) : S with type elt = Eq.t
(** Functor building an implementation of the set structure
    given an equality type. *)
