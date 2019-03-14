open Operation
open Exceptions

module Make(Val: Irmin.Contents.S) = struct
  module Op = Operation.Make(Val)
  module OpSet = Set.Make(Op)

  (* A description is a set of operations *)
  type 'i t = OpSet.t

  let describe unboxed = Op.B unboxed

  let rec interface_to_list: type i p. (Val.t, i) interface -> Op.t list = fun interface ->
    match interface with
    | Unary t -> [Op.B t]
    | Complex (t, ts) -> (Op.B t)::interface_to_list(ts)

  let (@) i is = Complex (i, is)

  let finally op = Unary op

  let define: (Val.t, 'i) interface -> 'i t = fun interface ->
    let l = interface_to_list interface in
    let len = List.length l in
    let set = OpSet.of_list l in

    if (OpSet.cardinal set != len) then
      raise @@ Invalid_description "Duplicate function name contained in list"
    else set

  let valid_name name d =
    OpSet.exists (fun b -> match b with
        | Op.B unboxed -> (NamedOp.name unboxed) == name) d
end

module type S = sig
  module Val: Irmin.Contents.S
  type shape
  val api: shape Make(Val).t
end
