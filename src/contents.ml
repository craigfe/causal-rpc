(* A task is a key and an operation to perform on the associated binding *)
type task = string * Interface.Description.op

(* A job is a branch name *)
type job = string

let job = Irmin.Type.string

let task =
  let open Irmin.Type in
  pair string string

type 'v contents =
  | Value of 'v
  | Task_queue of (task list * task list)
  | Job_queue of job list

module MakeContents (Val: Irmin.Contents.S) : Irmin.Contents.S
  with type t = Val.t contents = struct

  type t = Val.t contents

  let t =
    let open Irmin.Type in
    variant "contents" (fun value task_queue branch_name -> function
        | Value v -> value v
        | Task_queue q -> task_queue q
        | Job_queue js -> branch_name js)
    |~ case1 "Value" Val.t (fun v -> Value v)
    |~ case1 "Task_queue" (pair (list task) (list task)) (fun q -> Task_queue q)
    |~ case1 "Job_queue" (list job) (fun js -> Job_queue js)
    |> sealv

  let pp = Irmin.Type.pp_json t

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    Irmin.Type.decode_json t decoder

  let merge = Irmin.Merge.(option (idempotent t))
end
