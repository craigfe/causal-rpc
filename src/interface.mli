(** A set of RPC implementations indexed by string names *)
module Implementation : sig
  type operation_key = string

  (** The type of implementations of functions from type 'a to 'a *)
  type 'a t

  (** Generate an implementation from a hashtable *)
  val of_hashtable: (string, ('a -> 'a)) Hashtbl.t -> 'a t

  (** Retreive an operation from an implementation *)
  val find_operation_opt: operation_key -> 'a t -> ('a -> 'a) option
end

(** A set of RPC operations *)
module Description : sig
  type op = string

  (** The type of descriptions *)
  type t

  (** Generate a description from a set *)
  val of_set: Set.Make(String).t -> t

  (** Test whether or not an operation is contained in the description *)
  val valid_operation: t -> op -> bool
end

module type DESC = sig
  type t
  val api: Description.t
end

module type IMPL = sig
  type t
  val api: t Implementation.t
end

exception Invalid_definition of string

module type S = sig
  val declare: string list -> Description.t
  val implement: (string * ('a -> 'a)) list -> 'a Implementation.t
end

module Make: S

