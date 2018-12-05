
module Int: Irmin.Contents.S with type t = int64 = struct
  type t = int64
  let t = Irmin.Type.int64

  let merge = Irmin.Merge.(option (default t))
end


module O = Interface.MakeOperation(Int)
open O

let identity_op = declare "identity" return
let increment_op = declare "increment" return
let multiply_op = declare "multiply" Type.(int64 @-> return)
let complex_op = declare "complex" Type.(int32 @-> int64 @-> string @-> unit @-> return)

module Definition = struct
  module Val = Int
  module D = Interface.Description(Int)
  open D

  let api = define [
      describe identity_op;
      describe increment_op;
      describe complex_op; (* Note: the order of definition doesn't matter *)
      describe multiply_op;
    ]
end

module Implementation: Interface.IMPL with type Val.t = int64 = struct
  module Val = Int
  module I = Interface.MakeImplementation(Val)
  open I

  let identity x = x
  let increment x = Int64.add Int64.one x
  let multiply = Int64.mul
  let complex i32 i64 s () = match Int64.of_string_opt s with
    | Some i -> Int64.mul (Int64.mul (Int64.mul (Int64.of_int32 i32) i64) i)
    | None -> Int64.mul Int64.minus_one

  let api = define [
      implement identity_op identity;
      implement increment_op increment;
      implement multiply_op multiply;
      implement complex_op complex
    ]
end

module IntMap = Map.Make
    (Definition)
    (Job_queue.Type)
    (Job_queue.Make)

module IntWorker = Worker.Make(IntMap)(Implementation)
