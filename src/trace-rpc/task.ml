type t = {
  name: string; [@compare fun _ _ -> 0]
  params: Type.Boxed.t list; [@compare fun _ _ -> 0]
  key: string;
} [@@deriving eq, ord]

let show t = t.key
let pp = Fmt.using show Fmt.string

let t =
  let open Irmin.Type in
  record "task" (fun name params key -> { name; params; key })
  |+ field "name" string (fun t -> t.name)
  |+ field "params" (list Type.Boxed.irmin_t) (fun t -> t.params)
  |+ field "key" string (fun t -> t.key)
  |> sealr

let of_rpc key (Remote.{name; params}: 'v Remote.rpc) =
  {name; params; key}
