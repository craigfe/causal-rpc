open Lwt.Infix

exception Empty_queue

module Type = struct
  type t = string list
  type job = string

  let t = Irmin.Type.list Irmin.Type.string
  let job = Irmin.Type.string
end

module Make
    (Val: Irmin.Contents.S)
    (St: Irmin.KV with type contents = (Val.t, Type.t) Map.contents): Map.JOB_QUEUE with module Store = St = struct

  type t = string list
  type job = string

  module Type = Type
  module Store = St

  module type IMPL = sig
    val job_of_string: string -> job
    val job_to_string: job -> string

    val is_empty: Store.t -> bool Lwt.t
    val push: job -> Store.t -> unit Lwt.t
    val pop: Store.t -> job Lwt.t
    val peek_opt: Store.t -> job option Lwt.t
  end

  module Impl = struct
    let job_of_string j = j
    let job_to_string j = j

    let of_map m =
      Store.find m ["job_queue"]
      >|= fun q -> match q with
      | Some Trace_rpc.Map.Job_queue js -> js
      | Some _ -> invalid_arg "Can't happen by design"
      | None -> []

    let is_empty m =
      of_map m
      >|= fun js -> not (List.exists (fun _ -> true) js)

    let push j m = (* TODO: make this atomic *)
      of_map m
      >>= fun js -> Store.set m ~info:(Irmin_unix.info ~author:"map" "Issuing map")
        ["job_queue"] (Job_queue (j::js))

    (* TODO: make sure the right job is popped *)
    let pop m = (* TODO: make this atomic *)
      of_map m
      >>= fun js -> match js with
      | (j::js) ->
        Store.set m ~info:(Irmin_unix.info ~author:"map" "Completed map")
                     ["job_queue"] (Job_queue js)
        >|= fun () -> j
      | [] -> raise Empty_queue

    let peek_opt m =
      of_map m
      >|= fun js -> match js with
      | (j::_) -> Some j
      | [] -> None
  end
end
