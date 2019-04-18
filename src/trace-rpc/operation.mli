(* TODO: hide these constructors when apply is implemented *)

type (_,_) params
type (_,_,_,_) prototype

module NamedOp: sig
  type ('v, 'a, 'p, 'd) t
  (** The type of operations on type T.t *)

  val name: ('v, 'a, 'p, 'd) t -> string
  (** Return the name of an operation *)

  val typ: ('v, 'a, 'p, 'd) t -> ('v, 'a, 'p, 'd) prototype
  (** Return the name of an operation *)
end

(** A heterogeneous list of named operations *)
type (_,_) interface =
  | Unary : ('v,'a,'p,'d) NamedOp.t -> ('v,'a) interface
  | Complex : (('v,'a,'p,'d) NamedOp.t * ('v, 'b) interface) -> ('v, 'a * 'b) interface

(** An interface and a corresponding heterogeneous list of functions *)
type ('v,'a) implementation = ('v,'a) interface * 'a

module type S = sig
  module Val: Irmin.Contents.S

  type t = | B: (Val.t, 'a, 'p, 'd) NamedOp.t -> t
  type ('a, 'p, 'd) matched_implementation = (Val.t, 'a, 'p, 'd) NamedOp.t * 'a
  type boxed_mi = | E: ('a, 'p, 'd) matched_implementation -> boxed_mi

  (** Box a heterogeneous list to a serialisable form *)
  val flatten_params: (Val.t, 'a) params -> Type.Boxed.t list

  (* Take a list of parameters and apply them to a function *)
  val pass_params: ?src:Logs.src -> boxed_mi -> Type.Boxed.box list -> Val.t -> Val.t Lwt.t

  val apply: ('v, 'a, 'p, 'd) NamedOp.t -> 'p
  val app: ('v, 'a, 'p, 'd) NamedOp.t -> 'd
  val return: ('a, 'a -> 'a, 'a Remote.t, 'a Remote.rpc) prototype
  val return_lwt: ('a, 'a -> 'a Lwt.t, 'a Remote.t, 'a Remote.rpc) prototype

  val (@->): 't Type.t
    -> ('v, 'a, 'p, 'd) prototype
    -> ('v, 't -> 'a, 't -> 'p, 't -> 'd) prototype
  (** Combinator for describing functional types *)

  val declare: string -> (Val.t, 'b, 'p, 'd) prototype -> (Val.t, 'b, 'p, 'd) NamedOp.t
  (** Declare a function with a name and a number of arguments *)

  val compare: t -> t -> int
end

module Make(St: Irmin.Contents.S): S with module Val = St
