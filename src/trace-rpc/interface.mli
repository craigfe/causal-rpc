type (_,_) func_type =
  | BaseType : ('a, 'a -> 'a) func_type
  | ParamType : ('t Type.t * ('a, 'b) func_type) -> ('a, ('t -> 'b)) func_type

type (_,_) params =
  | Unit : ('v, 'v -> 'v) params
  | Param : ('p Type.t * 'p * ('v,'a) params) -> ('v, 'p -> 'a) params

module NamedOp: sig
  type ('v, 'a) t
  (** The type of operations on type T.t *)

  val name: ('v, 'a) t -> string
  (** Return the name of an operation *)

  val typ: ('v, 'a) t -> ('v, 'a) func_type
  (** Return the name of an operation *)
end

(** A heterogeneous list of named operations *)
type (_,_) interface =
  | Unary : ('v,'a) NamedOp.t -> ('v,'a) interface
  | Complex : (('v,'a) NamedOp.t * ('v, 'b) interface) -> ('v, 'a * 'b) interface

(** An interface and a corresponding heterogeneous list of functions *)
type ('v,'a) implementation = ('v,'a) interface * 'a

module type OPERATION = sig
  module Val: Irmin.Contents.S

  type t = | B: (Val.t, 'a) NamedOp.t -> t
  type 'a matched_implementation = (Val.t, 'a) NamedOp.t * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  (* val apply: ('v,'a) interface -> 'a -> (('v,'a) interface * ('v, 'a) params) *)
  val return: ('a, 'a -> 'a) func_type

  val (@->): 'p Type.t -> ('a, 'b) func_type -> ('a, 'p -> 'b) func_type
  (** Combinator for describing functional types *)

  val declare: string -> (Val.t, 'b) func_type -> (Val.t, 'b) interface
  (** Declare a function with a name and a number of arguments *)

  val compare: t -> t -> int
end

module MakeOperation(St: Irmin.Contents.S): OPERATION with module Val = St

(** Returned if a description or an implementation cannot be created *)
exception Invalid_description of string

(** A set of RPC operations *)
module Description(S: Irmin.Contents.S) : sig
  module Op: OPERATION

  type 'i t
  (** The type of descriptions over type 'a *)

  val (@): (Op.Val.t,'a) interface
    -> (Op.Val.t, 'b) interface
    -> (Op.Val.t, 'a * 'b) interface

  val describe: (Op.Val.t, 'a) NamedOp.t -> Op.t

  val define: (Op.Val.t,'i) interface -> 'i t
  (** Construct an RPC interface description from a list of declared functions *)

  val valid_name: string -> 'i t -> bool
  (** Test whether or not an operation is contained in the description *)

end with module Op = MakeOperation(S)

module type IMPL_MAKER = sig
  module S: Irmin.Contents.S
  module Op: OPERATION with module Val = S

  type 'i t
  (** The type of implementations of functions from type 'a to 'a *)

  val (@): (Op.Val.t, 'a) implementation
    -> (Op.Val.t, 'b) implementation
    -> (Op.Val.t, 'a * 'b) implementation

  val define: (Op.Val.t,'i) implementation -> 'i t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> 'i t -> Op.boxed_mi option
  (** Retreive an operation from an implementation *)
end

module MakeImplementation(T: Irmin.Contents.S) : IMPL_MAKER
  with module S = T
   and module Op = MakeOperation(T)


module type DESC = sig
  module Val: Irmin.Contents.S
  type shape
  val api: shape Description(Val).t
end


module type IMPL = sig
  module Val: Irmin.Contents.S
  type shape
  val api: shape MakeImplementation(Val).t
end
