(* TODO: hide these constructors when apply is implemented *)
type (_,_) params =
  | Unit : ('v, 'v -> 'v) params
  | Param : ('p Type.t * 'p * ('v,'a) params) -> ('v, 'p -> 'a) params

type (_,_,_) prototype =
  | BaseType : ('a, 'a -> 'a, ('a, 'a -> 'a) params) prototype
  | ParamType : ('t Type.t * ('a, 'b, ('a, 'p) params) prototype)
      -> ('a, ('t -> 'b), ('a, 't -> 'p) params) prototype

module NamedOp: sig
  type ('v, 'a, 'p) t
  (** The type of operations on type T.t *)

  val name: ('v, 'a, 'p) t -> string
  (** Return the name of an operation *)

  val typ: ('v, 'a, 'p) t -> ('v, 'a, 'p) prototype
  (** Return the name of an operation *)
end

(** A heterogeneous list of named operations *)
type (_,_) interface =
  | Unary : ('v,'a,'p) NamedOp.t -> ('v,'a) interface
  | Complex : (('v,'a,'p) NamedOp.t * ('v, 'b) interface) -> ('v, 'a * 'b) interface

(** An interface and a corresponding heterogeneous list of functions *)
type ('v,'a) implementation = ('v,'a) interface * 'a

module type S = sig
  module Val: Irmin.Contents.S

  type t = | B: (Val.t, 'a, 'p) NamedOp.t -> t
  type ('a, 'p) matched_implementation = (Val.t, 'a, 'p) NamedOp.t * 'a
  type boxed_mi = | E: ('a, 'p) matched_implementation -> boxed_mi

  (** Box a heterogeneous list to a serialisable form *)
  val flatten_params: (Val.t, 'a) params -> Type.Boxed.t list

  (* Take a list of parameters and apply them to a function *)
  val pass_params: ?src:Logs.src -> boxed_mi -> Type.Boxed.box list -> Val.t -> Val.t

  (* val apply: ('v,'a) interface -> 'a -> (('v,'a) interface * ('v, 'a) params) *)
  val return: ('a, 'a -> 'a, ('a, 'a -> 'a) params) prototype

  val (@->): 't Type.t
    -> ('a, 'b, ('a, 'p) params) prototype
    -> ('a, ('t -> 'b), ('a, 't -> 'p) params) prototype
  (** Combinator for describing functional types *)

  val declare: string -> (Val.t, 'b, 'p) prototype -> (Val.t, 'b, 'p) NamedOp.t
  (** Declare a function with a name and a number of arguments *)

  val compare: t -> t -> int
end

module Make(St: Irmin.Contents.S): S with module Val = St