module Param : sig
  type t
  (** The type of parameters *)

  val t: t Irmin.Type.t
  (** Irmin type for operations *)

  val test_t: t Alcotest.testable
end

module Operation : sig

  type t
  (** The type of operations *)

  val t: t Irmin.Type.t
  (** Irmin type for operations *)

  val test_t: t Alcotest.testable

  val name: t -> string
  (** Return the name of an operation *)

  val arity: t -> int
  (** Return the number of arguments an operation takes *)

  val declare: string -> int -> t
  (** Declare a function with a name and a number of arguments *)
end

(** Returned if a description or an implementation cannot be created *)
exception Invalid_description of string

(** A set of RPC operations *)
module Description : sig

  type 'a t
  (** The type of descriptions *)

  val define: Operation.t list -> 'a t
  (** Construct an RPC interface description from a list of declared functions *)

  val valid_name: string -> 'a t -> bool
  (** Test whether or not an operation is contained in the description *)
end


module Implementation : sig
  type operation_key = string

  (** The type of implementations of functions from type 'a to 'a *)
  type 'a t

  val implement: (Operation.t * (Param.t list -> 'a -> 'a)) list -> 'a t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  (** Retreive an operation from an implementation *)
  val find_operation_opt: Operation.t -> 'a t -> (Param.t list -> 'a -> 'a) option
end

module type DESC = sig
  type t
  val api: t Description.t
end

module type IMPL = sig
  type t
  val api: t Implementation.t
end
