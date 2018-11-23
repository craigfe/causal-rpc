open Type

type (_,_) func_type =
  | BaseType : ('a, 'a -> 'a) func_type
  | ParamType : (unit * ('a, 'b) func_type) -> ('a, (Param.t -> 'b)) func_type

type (_,_) params_gadt =
  | Unit : ('v, 'v -> 'v) params_gadt
  | Param : (Param.t * ('v,'a) params_gadt) -> ('v, Param.t -> 'a) params_gadt

module type OPERATION = sig
  module Val: Irmin.Contents.S

  type 'a unboxed
  (** The type of operations on type T.t *)

  type 'a params = (Val.t, 'a) params_gadt
  type t = | B: 'a unboxed -> t

  type 'a matched_implementation = 'a unboxed * 'a

  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  val name: 'a unboxed -> string
  (** Return the name of an operation *)

  val typ: 'a unboxed -> (Val.t, 'a) func_type
  (** Return the type of an operation *)

  val return: ('a, 'a -> 'a) func_type

  val (-->): unit -> ('a, 'b) func_type -> ('a, Param.t -> 'b) func_type

  val declare: string -> (Val.t, 'b) func_type -> 'b unboxed
  (** Declare a function with a name and a number of arguments *)

  val compare: t -> t -> int
end

module MakeOperation(St: Irmin.Contents.S): OPERATION with module Val = St

(** Returned if a description or an implementation cannot be created *)
exception Invalid_description of string

(** A set of RPC operations *)
module Description(S: Irmin.Contents.S) : sig
  module Op: OPERATION

  type t
  (** The type of descriptions over type 'a*)

  val describe: 'a Op.unboxed -> Op.t

  val define: Op.t list -> t
  (** Construct an RPC interface description from a list of declared functions *)

  val valid_name: string -> t -> bool
  (** Test whether or not an operation is contained in the description *)

end with module Op = MakeOperation(S)

module type IMPL_MAKER = sig
  module S: Irmin.Contents.S
  module Op: OPERATION with module Val = S

  type t
  (** The type of implementations of functions from type 'a to 'a *)

  val implement: 'a Op.unboxed -> 'a -> Op.boxed_mi

  val define: Op.boxed_mi list -> t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> t -> Op.boxed_mi option
  (** Retreive an operation from an implementation *)
end

module MakeImplementation(T: Irmin.Contents.S) : IMPL_MAKER
  with module S = T
   and module Op = MakeOperation(T)


module type DESC = sig
  module Val: Irmin.Contents.S
  val api: Description(Val).t
end


module type IMPL = sig
  module Val: Irmin.Contents.S
  val api: MakeImplementation(Val).t
end
