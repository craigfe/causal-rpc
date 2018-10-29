module Param = struct
  type t = string Irmin.Type.t
  let mk name = name
end

type rpc = {
  id: string;
  params: string list;
  result: string option;
}

let rpc_t =
    let open Irmin.Type in
    record "rpc" (fun id params result -> { id; params; result })
    |+ field "id"  string (fun t -> t.id)
    |+ field "params" (list (Param.mk string)) (fun t -> t.params)
    |+ field "result" (option (Param.mk string)) (fun t -> t.result)
    |> sealr

module Rpc = struct
  type t = rpc
  let t = rpc_t

  let pp = Irmin.Type.pp_json t
  let of_string s = Irmin.Type.decode_json t (Jsonm.decoder (`String s))

  let make id params result = {id; params; result}
end
