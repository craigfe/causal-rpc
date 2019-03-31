open Operation

module type IMPL_MAKER = sig
  module S: Irmin.Contents.S
  module Op: Operation.S with module Val = S

  type 'i t
  (** The type of implementations of functions from type 'a to 'a *)

  val (@): ((Op.Val.t, 'a, 'p, 'd) NamedOp.t * 'a)
    -> (Op.Val.t, 'b) implementation
    -> (Op.Val.t, 'a * 'b) implementation

  val finally: ((Op.Val.t, 'a, 'p, 'd) NamedOp.t * 'a) -> (Op.Val.t, 'a) implementation

  val define: (Op.Val.t, 'i) Operation.implementation -> 'i t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> 'i t -> Op.boxed_mi option
  (** Retreive an operation from an implementation *)
end

module MakeImplementation(T: Irmin.Contents.S) : IMPL_MAKER
  with module S = T
   and module Op = Operation.Make(T)


module type IMPL = sig
  module Val: Irmin.Contents.S
  type shape
  val api: shape MakeImplementation(Val).t
end
