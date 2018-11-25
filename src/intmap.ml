
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

let increment_op = declare "increment" return
let multiply_op = declare "multiply" Type.(int64 @-> return)
let complex_op = declare "complex" Type.(int32 @-> int64 @-> string @-> unit @-> return)

module Definition = struct
  module Val = Int
  module I = Interface.Description(Int)
  open I

  (* TODO: remove. This shouldn't be necessary*)
  let increment_op = increment_op
  let multiply_op = multiply_op
  let complex_op = complex_op


  let api = define [
      describe increment_op;
      describe complex_op; (* Note: the order of definition doesn't matter *)
      describe multiply_op;
    ]
end

module Implementation: Interface.IMPL with type Val.t = int64 = struct
  module Val = Int
  module I = Interface.MakeImplementation(Val)
  open I

  let increment = Int64.add Int64.one
  let multiply = Int64.mul
  let complex i32 i64 s () = match Int64.of_string_opt s with
    | Some i -> Int64.mul (Int64.mul (Int64.mul (Int64.of_int32 i32) i64) i)
    | None -> Int64.mul Int64.minus_one

  let api = define [
      implement increment_op increment;
      implement multiply_op multiply;
      implement complex_op complex
    ]
end

module IntMap = Map.Make
    (Definition)
    (Irmin_unix.Git.FS.KV)
    (Job_queue.Type)
    (Job_queue.Make)

module IntWorker = Worker.Make(IntMap)(Implementation)
