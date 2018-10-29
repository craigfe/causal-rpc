
module Version : sig
  type t = int
  val compare : t -> t -> int
end

type param =
  | String of string Irmin.Type.t
  | Int of int64 Irmin.Type.t

type named_param = string * param

type call = {name: string; params: named_param list}
type response = {contents: string}

val call : string -> named_param list -> call
val success: string -> response
