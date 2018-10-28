
module Version : sig
  type t = int
  val compare : t -> t -> int
end

type response = {contents: string}
type call = {name: string; params: string list}
val call : string -> string list -> call
