type t = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
} [@@deriving eq, ord]
(** The type of tasks *)

val show: t -> string
val pp: t Fmt.t

val t: t Irmin.Type.t
val of_rpc: string -> 'v Remote.rpc -> t
(* [of_rpc key rpc] constructs a task from a [Remote.rpc] value, intended
   for the key [key]*)
