type boxed_op = string * int32
type 'n op = string * 'n
type param = string

val boxed_op: boxed_op Irmin.Type.t
val param: param Irmin.Type.t

open Llist

type (_, _) func =
  | Function : param * ('n, 'b) func -> ('n s, param -> 'b) func
  | Returning : param -> (z, param) func
type 'a boxed_func = B: ('n, 'a) func -> 'a boxed_func

type ('n, 'a) implementation = 'n op * ('n, 'a) func
type 'a boxed_implementation = B: 'n op * ('n, 'a) func -> 'a boxed_implementation

exception Invalid_definition of string

module Operation : sig

  val declare: string -> int -> boxed_op
  (** Declare a function with a name and a number of arguments *)
end

(** A set of RPC implementations indexed by string names *)
module Implementation : sig

  (** The type of implementations of functions from type 'a to 'a *)
  type 'a t

  val implement: 'n op * ('n, 'a) func -> 'a boxed_implementation

  (** Retrieve an operation from an implementation *)
  val find_operation_opt: 'n op -> 'a t -> ('n, 'a) func option

  val seal: 'a boxed_implementation list -> 'a t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)
end

(** A set of RPC operations *)
module Description : sig
  type 'a t
  (** The type of descriptions of interfaces over values of type 'a *)

  val of_operation_list: 'a boxed_func list -> 'a t
  (** Construct an RPC interface description from a list of declared functions *)

  (** Test whether or not an operation is contained in the description *)
  val valid_name: string -> 'a t -> bool
end

module type DESC = sig
  type t
  val api: t Description.t
end

module type IMPL = sig
  type t
  val api: t Implementation.t
end


