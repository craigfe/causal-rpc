type task = string * Interface.Description.op

let task =
  let open Irmin.Type in
  pair string string


