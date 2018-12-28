open Lwt.Infix
open Map

exception Empty_queue
module Make
    (Info: Info.S)
    (Val: Irmin.Contents.S)
    (St: Store.S
     with type key = Irmin.Path.String_list.t
      and type step = string
      and module Key = Irmin.Path.String_list
      and type contents = Val.t Map.contents
      and type branch = string)

  : Map.JOB_QUEUE with module Store = St = struct

  type t = string list
  type job = string

  module Store = St

  module type IMPL = sig
    val job_of_string: string -> job
    val job_to_string: job -> string
    val job_equal: job -> job -> bool

    val is_empty: Store.t -> bool Lwt.t
    val push: job -> Store.t -> unit Lwt.t
    val pop: Store.t -> job Lwt.t
    val peek_opt: Store.t -> job option Lwt.t
  end

  module Impl = struct
    let job_of_string j = j
    let job_to_string j = j

    let job_equal a b = (a == b)

    let of_map m =
      Store.find m ["job_queue"]
      >|= fun q -> match q with
      | Some Map.Job_queue js -> js
      | Some _ -> invalid_arg "Can't happen by design"
      | None -> []

    let is_empty m =
      of_map m
      >|= fun js -> not (List.exists (fun _ -> true) js)

    let push j m = (* TODO: make this atomic *)
      of_map m
      >>= fun js -> Store.set m
        ~info:(Info.make ~author:"map" "Add %s to job queue" j)
        ["job_queue"]
        (Job_queue (j::js))
      >|= fun res -> match res with
      | Ok () -> ()
      | Error _ -> invalid_arg "some error"

    (* TODO: make sure the right job is popped *)
    let pop m = (* TODO: make this atomic *)
      of_map m
      >>= fun js -> match js with
      | (j::js) ->
        Store.set m
          ~info:(Info.make ~author:"map" "Remove %s from job queue" j)
          ["job_queue"]
          (Job_queue js)
        >|= fun res -> (match res with
        | Ok () -> j
        | Error _ -> invalid_arg "some error")
      | [] -> raise Empty_queue

    let peek_opt m =
      of_map m
      >|= fun js -> match js with
      | (j::_) -> Some j
      | [] -> None
  end
end
