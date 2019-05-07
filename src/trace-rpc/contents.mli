type 'v t =
  | Value of 'v
  | Task_queue of Task_queue.t
  | Job_queue of Job_queue.t
(** The set of values contained within a CauslRPC store, parameterised
    on the user-supplied data value 'v *)

module Make(Val: Irmin.Contents.S): Irmin.Contents.S
  with type t = Val.t t
