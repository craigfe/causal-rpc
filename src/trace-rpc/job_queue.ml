type t = Job.t list
let t = Irmin.Type.list Job.t

let is_empty js =
  not (List.exists (fun _ -> true) js)

let push = List.cons

let pop = function
  | (j::js) -> Ok (j, js)
  | _ -> Error "Empty job queue on pop"

let peek_opt = function
  | (j::_) -> Some j
  | [] -> None
