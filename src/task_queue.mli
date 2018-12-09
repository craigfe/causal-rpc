
type task = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
} [@@deriving show]

type t = (task list * task list) [@@deriving show]
val t: t Irmin.Type.t
val t_testable: t Alcotest.testable

val merge: t Irmin.Merge.f
