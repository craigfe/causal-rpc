(** A task is a key and an operation to perform on the associated binding **)
type task = string * Interface.Description.op

(** A job is a branch name **)
type job = string

type 'v contents =
  | Value of 'v
  | Task_queue of (task list * task list)
  | Job_queue of job list

module Make (Val: Irmin.Contents.S) : Irmin.Contents.S
  with type t = Val.t contents
