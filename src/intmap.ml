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

open Interface.Operation
let double_op = declare "double" 0
let increment_op = declare "increment" 0

module Definition: Interface.DESC with type t = int64 = struct
  open Interface.Description

  type t = int64

  let api = define [ double_op; increment_op ]
end

module Implementation: Interface.IMPL with type t = int64 = struct
  open Interface.Implementation

  type t = int64

  let increment = Int64.add Int64.one
  let double = Int64.mul (Int64.of_int 2)

  let api = implement [
      (increment_op, fun _ -> increment);
      (double_op, fun _ -> double)
    ]
end

module IntMap = Map.Make(Int)(Definition)(Job_queue.Type)(Job_queue.Make)
module IntWorker = Worker.Make(IntMap)(Implementation)
