open Lwt.Infix

type t = string list
exception Empty_queue

module Make
    (Val: Irmin.Contents.S)
    (Store: Irmin.KV with type contents = Map_contents.Make(Val).t) = struct

  open Map_contents

  let of_map m =
    Store.find m ["job_queue"]
    >|= fun q -> match q with
    | Some Job_queue js -> js
    | Some _ -> invalid_arg "Can't happen by design"
    | None -> []

  let push j m = (* TODO: make this atomic *)
    of_map m
    >>= fun js -> Store.set m ~info:(Irmin_unix.info ~author:"map" "Issuing map")
      ["job_queue"] (Job_queue (j::js))

  (* TODO: make sure the right job is popped *)
  let pop m = (* TODO: make this atomic *)
    of_map m
    >>= fun js -> match js with
    | (_::js) -> Store.set m ~info:(Irmin_unix.info ~author:"map" "Completed map")
                   ["job_queue"] (Job_queue js)
    | [] -> raise Empty_queue
end
