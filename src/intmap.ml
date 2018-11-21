
module Int: Irmin.Contents.S with type t = int64 = struct
  type t = int64
  let t = Irmin.Type.int64

  let pp fmt = Format.fprintf fmt "%Ld"

  let of_string s =
    match Int64.of_string_opt s with
    | Some i -> Ok i
    | None -> Error (`Msg "invalid counter value")

  let merge ~old a b =
    let open Irmin.Merge.Infix in
    old () >|=* fun old ->
    let old = match old with None -> 0L | Some o -> o in
    let (+) = Int64.add and (-) = Int64.sub in
    a + b - old

  let merge = Irmin.Merge.(option (v t merge))
end


module O = Interface.Operation(Int)
open O

let double_op = declare "double" return
let increment_op = declare "increment" return
let multiply_op = declare "multiply" (() --> return)

module Definition: Interface.DESC with type S.t = int64 = struct
  module I = Interface.Description(Int)
  open I

  module S = Int

  let api = define [
      describe double_op;
      describe increment_op;
      describe multiply_op
    ]
end

module Implementation: Interface.IMPL with type S.t = int64 = struct
  module I = Interface.Implementation(Int)
  open I

  module S = Int

  let increment = Int64.add Int64.one
  let double = Int64.mul (Int64.of_int 2)

  let multiply x = match x with
    | Interface.Param.Int64 i -> Int64.mul i
    | _ -> invalid_arg "Wrong argument type"

  let api = define [
      implement increment_op increment;
      implement double_op double;
      implement multiply_op multiply
    ]
end

module IntMap = Map.Make
    (Definition)
    (Irmin_unix.Git.FS.KV)
    (Job_queue.Type)
    (Job_queue.Make)

module IntWorker = Worker.Make(IntMap)(Implementation)
