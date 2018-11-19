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

module Definition(I: Interface.S): Interface.DESC with type t = int64 = struct
  open I
  type t = int64

  let api = describe [
      declare "increment" Int32.zero;
      declare "double" Int32.zero
    ]
end

module Implementation(I: Interface.S): Interface.IMPL with type t = int64 = struct
  open I
  type t = int64

  let increment = Int64.add Int64.one
  let double = Int64.mul (Int64.of_int 2)

  let api = implement [
      (declare "increment" Int32.zero, fun _ -> increment);
      (declare "double"    Int32.zero, fun _ -> double)
    ]
end

module I = Interface.Make
module IntMap = Map.Make(Int)(Definition(I))(Job_queue.Type)(Job_queue.Make)
module IntWorker = Worker.Make(IntMap)(Implementation(I))
