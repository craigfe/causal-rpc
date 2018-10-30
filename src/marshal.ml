module Id = struct
  type t = int64
  let id_of_int x = Int64.of_int x
  let id_of_int64 x = x
end

module Param = struct
  type 'a t = 'a
  let t = Irmin.Type.string

end

type rpc = {
  id: Id.t;
  params: string Param.t list;
  result: string Param.t option;
}

let rpc_t =
  let open Irmin.Type in
    record "rpc" (fun id params result -> { id; params; result })
    |+ field "id" int64 (fun t -> t.id)
    |+ field "params" (list Param.t) (fun t -> t.params)
    |+ field "result" (option Param.t) (fun t -> t.result)
    |> sealr

module Rpc = struct
  type t = rpc
  let t = rpc_t

  let pp = Irmin.Type.pp_json t
  let of_string s = Irmin.Type.decode_json t (Jsonm.decoder (`String s))

  let make id params result = {id; params; result}
end
