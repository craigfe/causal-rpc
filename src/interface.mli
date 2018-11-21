
module Param : sig
  type t =
    | Unit of unit
    | Bool of bool
    | Char of char
    | Int32 of int32
    | Int64 of int64
    | String of string

  val irmin_t: t Irmin.Type.t
end

type (_,_) func_type =
  | BaseType : ('a, 'a -> 'a) func_type
  | ParamType : (unit * ('a, 'b) func_type) -> ('a, (Param.t -> 'b)) func_type

type (_,_) func =
  | Base : ('a -> 'a) -> ('a, 'a -> 'a) func
  | Param : (Param.t -> ('a, 'b) func) -> ('a, (Param.t -> 'b)) func

type (_,_) params_gadt =
  | V : ('v, 'v -> 'v) params_gadt
  | P : (Param.t * ('v,'a) params_gadt) -> ('v, Param.t -> 'a) params_gadt

module type OPERATION = sig
  type value

  type 'a unboxed
  (** The type of operations on type T.t *)

  type 'a params = (value, 'a) params_gadt
  type t = | B: 'a unboxed -> t

  type 'a matched_implementation = 'a unboxed * 'a

  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  val name: 'a unboxed -> string
  (** Return the name of an operation *)

  val typ: 'a unboxed -> (value, 'a) func_type
  (** Return the type of an operation *)

  val return: ('a, 'a -> 'a) func_type

  val (-->): unit -> ('a, 'b) func_type -> ('a, Param.t -> 'b) func_type

  val declare: string -> (value, 'b) func_type -> 'b unboxed
  (** Declare a function with a name and a number of arguments *)

  val compare: t -> t -> int
end

module Operation(S: Irmin.Contents.S): OPERATION with type value = S.t

(** Returned if a description or an implementation cannot be created *)
exception Invalid_description of string

(** A set of RPC operations *)
module Description(S: Irmin.Contents.S) : sig

  type t
  (** The type of descriptions over type 'a*)

  val describe: 'a Operation(S).unboxed -> Operation(S).t

  val define: Operation(S).t list -> t
  (** Construct an RPC interface description from a list of declared functions *)

  val valid_name: string -> t -> bool
  (** Test whether or not an operation is contained in the description *)

end


module Implementation(S: Irmin.Contents.S) : sig

  type t
  (** The type of implementations of functions from type 'a to 'a *)

  val implement: 'a Operation(S).unboxed -> 'a -> Operation(S).boxed_mi

  val define: Operation(S).boxed_mi list -> t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> t -> Operation(S).boxed_mi option
  (** Retreive an operation from an implementation *)

end

module type DESC = sig
  module S: Irmin.Contents.S
  val api: Description(S).t
end


module type IMPL = sig
  module S: Irmin.Contents.S
  val api: Implementation(S).t
end
