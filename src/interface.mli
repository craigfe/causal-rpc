type op = string * int32
type param = string

val op: op Irmin.Type.t
val param: param Irmin.Type.t

module Operation: Set.OrderedType with type t = op

(** A set of RPC implementations indexed by string names *)
module Implementation : sig
  type operation_key = string

  (** The type of implementations of functions from type 'a to 'a *)
  type 'a t

  (** Generate an implementation from a hashtable *)
  val of_hashtable: (op, (param list -> 'a -> 'a)) Hashtbl.t -> 'a t

  (** Retreive an operation from an implementation *)
  val find_operation_opt: op -> 'a t -> (param list -> 'a -> 'a) option
end

(** A set of RPC operations *)
module Description : sig

  type t
  (** The type of descriptions *)

  (** Generate a description from a set *)
  val of_set: Set.Make(Operation).t -> t

  (** Test whether or not an operation is contained in the description *)
  val valid_name: string -> t -> bool
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
  val declare: string -> int32 -> op
  (** Declare a function with a name and a number of arguments *)

  val describe: op list -> Description.t
  (** Construct an RPC interface description from a list of declared functions *)

  val implement: (op * (param list -> 'a -> 'a)) list -> 'a Implementation.t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)
end

module Make: S

