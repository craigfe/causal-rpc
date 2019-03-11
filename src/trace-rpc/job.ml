type t =
  | Rpc of (Task.t * string)
  | MapJob of string [@@deriving eq, show]

let t =
  let open Irmin.Type in
  variant "job" (fun rpc mapjob -> function
      | Rpc t -> rpc t
      | MapJob j -> mapjob j)
  |~ case1 "Rpc" (pair Task.t string) (fun t -> Rpc t)
  |~ case1 "MapJob" string (fun x -> MapJob x)
  |> sealv
