open Lwt.Infix
open Contents

exception Empty_queue

module Make
    (Val: Irmin.Contents.S)
    (B: Backend.S
        with type Store.key = Irmin.Path.String_list.t
         and type Store.step = string
         and module Store.Key = Irmin.Path.String_list
         and type Store.contents = Val.t Contents.t
         and type Store.branch = string)
  : Contents.JOB_QUEUE with module Store = B.Store = struct

  module Store = B.Store

  module type IMPL = sig
    val is_empty: Store.t -> bool Lwt.t
    val push: Job.t -> Store.t -> unit Lwt.t
    val pop: Store.t -> Job.t Lwt.t
    val pop_silent: Store.t -> (Job.t * Job.t list) Lwt.t
    val peek_opt: Store.t -> Job.t option Lwt.t
  end

  module Impl = struct
    let of_map m =
      Store.find m ["job_queue"]
      >|= fun q -> match q with
      | Some Job_queue js -> js
      | Some _ -> invalid_arg "Can't happen by design"
      | None -> []

    let is_empty m =
      of_map m
      >|= fun js -> not (List.exists (fun _ -> true) js)

    let push j m = (* TODO: make this atomic *)
      of_map m
      >>= fun js -> Store.set m
        ~info:(B.make_info ~author:"map" "Add %a to job queue" Job.pp j)
        ["job_queue"]
        (Contents.Job_queue (j::js))
      >|= fun res -> match res with
      | Ok () -> ()
      | Error _ -> invalid_arg "some error"

    (* TODO: make sure the right job is popped *)
    let pop m = (* TODO: make this atomic *)
      of_map m
      >>= fun js -> match js with
      | (j::js) ->
        Store.set m
          ~info:(B.make_info ~author:"map" "Remove %a from job queue" Job.pp j)
          ["job_queue"]
          (Job_queue js)
        >|= fun res -> (match res with
        | Ok () -> j
        | Error _ -> invalid_arg "some error")
      | [] -> raise Empty_queue

    let pop_silent m =
      of_map m
      >|= fun js -> match js with
      | (j::js) -> (j, js)
      | [] -> raise Empty_queue

    let peek_opt m =
      of_map m
      >|= fun js -> match js with
      | (j::_) -> Some j
      | [] -> None
  end
end
