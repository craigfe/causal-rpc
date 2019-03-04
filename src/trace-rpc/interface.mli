type 'a rpc
(** An operation with functional type 'a *)

type (_,_) func_type =
  | BaseType : ('a, ('a -> 'a) rpc)  func_type
  | ParamType : ('t Type.t * ('a, 'b) func_type) -> ('a, ('t -> 'b)) func_type

type (_,_) params_gadt =
  | Unit : ('v, 'v -> 'v) params_gadt
  | Param : ('p Type.t * 'p * ('v,'a) params_gadt) -> ('v, 'p -> 'a) params_gadt

module type OPERATION = sig
  module Val: Irmin.Contents.S

  module Unboxed: sig
    type 'a t
    (** The type of operations on type T.t *)

    val name: 'a t -> string
    (** Return the name of an operation *)

    val typ: 'a t -> (Val.t, 'a) func_type
    (** Return the name of an operation *)
  end

  type 'a params = (Val.t, 'a) params_gadt
  type t = | B: 'a Unboxed.t -> t

  type 'a matched_implementation = 'a Unboxed.t * 'a

  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  (* Take an operation and resolve it to an executable RPC by boxing the parameters *)
  val apply: 'a Unboxed.t -> 'a rpc

  (* Given a parameter *)
  val (@->): 'p Type.t -> ('a, 'b) func_type -> ('a, 'p -> 'b) func_type

  val declare: string -> (Val.t, 'b) func_type -> ('b -> )
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

  val describe: 'a Op.Unboxed.t -> Op.t

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

  val implement: 'a Op.Unboxed.t -> 'a -> Op.boxed_mi

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
