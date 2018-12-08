
type task = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
} [@@deriving show]

type t = (task list * task list) [@@deriving show]
val t: t Irmin.Type.t

val merge: t Irmin.Merge.f
