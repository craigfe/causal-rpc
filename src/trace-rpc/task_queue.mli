type t = (Task.t list * Task.t list) [@@deriving show]
val t: t Irmin.Type.t
val t_testable: t Alcotest.testable

val merge: t Irmin.Merge.f

(* Purely to stop OCaml complaining about unused values *)
module TaskSet: Set.S

type operations = {
  consume: TaskSet.t; [@printer pp_taskset]
  perform: TaskSet.t; [@printer pp_taskset]
} [@@deriving show]
