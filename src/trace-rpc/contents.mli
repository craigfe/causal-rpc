(* Helper types and values for a CauslRPC store *)
type 'v t =
  | Value of 'v
  | Task_queue of Task_queue.t
  | Job_queue of Job_queue.t

module Make(Val: Irmin.Contents.S): Irmin.Contents.S
  with type t = Val.t t
