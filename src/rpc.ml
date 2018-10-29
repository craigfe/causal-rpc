module Version = struct
  type t = int
  let compare a b = compare a b
end

type param = (* TODO: generalise to arbitrary Irmin.Type *)
  | String of string Irmin.Type.t
  | Int of int64 Irmin.Type.t

type named_param = string * param

type call = {name: string; params: named_param list}
type response = {contents: string}

let call name params = {name; params}
let success v = { contents = v }
