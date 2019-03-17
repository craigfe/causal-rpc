type t = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
} [@@deriving eq, ord]

val show: t -> string
val pp: t Fmt.t

val t: t Irmin.Type.t
val of_rpc: string -> 'v Operation.rpc -> t
