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

module Ops = struct
  type t = int64
  let iter = Int64.add Int64.one
end

module IntMap = Map.Make(Int)(Ops)
module IntWorker = Worker.Make(Int)(Ops)
