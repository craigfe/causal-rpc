
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


module O = Interface.MakeOperation(Int)
open O

let double_op = declare "double" return
let increment_op = declare "increment" return
let multiply_op = declare "multiply" (() --> return)

module Definition = struct
  module Val = Int
  module I = Interface.Description(Int)
  open I

  (* TODO: remove. This shouldn't be necessary*)
  let double_op = double_op
  let increment_op = increment_op
  let multiply_op = multiply_op


  let api = define [
      describe double_op;
      describe increment_op;
      describe multiply_op
    ]
end

module Implementation: Interface.IMPL with type Val.t = int64 = struct
  module Val = Int
  module I = Interface.MakeImplementation(Val)
  open I

  let increment = Int64.add Int64.one
  let double = Int64.mul (Int64.of_int 2)

  let multiply x = match x with
    | Type.Param.Int64 i -> Int64.mul i
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
