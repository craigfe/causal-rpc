open Lwt.Infix

(* A task is a key and an operation to perform on the associated binding *)
type task = string * string

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

(* Internal errors *)
exception Empty_queue

module type S = sig

  type key = string
  type value
  type operation = Interface.Description.op

  type t

  module Contents: Irmin.Contents.S with type t = value contents
  module Store: Irmin.KV with type contents = Contents.t
  module Sync: Irmin.SYNC with type db = Store.t

  val of_store: Sync.db -> t
  val empty: ?directory:string -> unit -> t
  val is_empty: t -> bool
  val mem: key -> t -> bool
  val add: key -> value -> t -> t
  val find: key -> t -> value
  val remove: key -> t -> t
  val size: t -> int
  val keys: t -> key list
  val values: t -> value list
  val map: operation -> t -> t
end

module Make (Val : Irmin.Contents.S) (Desc: Interface.DESC with type t = Val.t) = struct
  module Contents = MakeContents(Val)
  module Store = Irmin_unix.Git.FS.KV(Contents)
  module Sync = Irmin.Sync(Store)

  type key = string
  type value = Val.t
  type operation = Interface.Description.op

  type t = Sync.db
  (* A map is a branch in an Irmin Key-value store *)

  let generate_random_directory () =
    Misc.generate_rand_string ~length:20 ()
    |> Pervasives.(^) "/tmp/irmin/set/"
    |> fun x -> Logs.info (fun m -> m "No directory supplied. Generated random directory %s" x); x

  let empty ?(directory=generate_random_directory()) () =
    let config = Irmin_git.config ~bare:true directory in

    (* Delete the directory if it already exists... Unsafe! *)
    let ret_code = Sys.command ("rm -rf " ^ directory) in begin
      if (ret_code <> 0) then invalid_arg "Unable to delete directory";

      let lwt = Store.Repo.v config
        >>= fun repo -> Store.of_branch repo "master"
      in Lwt_main.run lwt
    end

  let of_store s = s

  let mem key m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.exists (fun (x,_) -> x = key)
    in Lwt_main.run lwt

  let add key value m =
    let lwt =
      Store.set m
        ~info:(Irmin_unix.info ~author:"test" "Committing to key %s" key)
        ["vals"; key]
        (Value value)
    in Lwt_main.run lwt; m

  let find key m =
    let lwt =
      (* Get the value from the store and deserialise it *)
      Store.get m ["vals"; key]
      >|= fun value -> match value with
      | Value v -> v
      | _ -> invalid_arg "Can't happen by design"

    in try
      Lwt_main.run lwt
    with Invalid_argument _ -> raise Not_found

  let remove _ _ = invalid_arg "TODO"

  let size m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      (* >|= List.filter (fun (_, typ) -> typ = `Contents) *)
      >|= List.length
    in Lwt_main.run lwt

  let is_empty m =
    (size m) == 0

  let keys m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.map(fst)
    in Lwt_main.run lwt

  let values m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >>= Lwt_list.map_p (fun (x, _) -> Store.get m ["vals"; x])
      >|= List.map (fun value -> match value with
          | Value v -> v
          | _ -> invalid_arg "Can't happen by design"
        )
    in Lwt_main.run lwt

  let get_job_queue m =
    Store.find m ["job_queue"]
    >|= fun q -> match q with
    | Some Job_queue js -> js
    | Some _ -> invalid_arg "Can't happen by design"
    | None -> []

  let get_task_queue m =
    Store.find m ["task_queue"]
    >|= fun q -> match q with
    | Some Task_queue tq -> tq
    | Some _ -> invalid_arg "Can't happen by design"
    | None -> ([], [])

  let task_queue_is_empty branch =
    let lwt =
      get_task_queue branch
      >|= fun q -> match q with
      | ([], []) -> true
      | _ -> false
    in Lwt_main.run lwt

  (* let job_queue_contains elt m =
   *   let lwt =
   *     get_job_queue m
   *     >|= List.exists (String.equal elt)
   *   in Lwt_main.run lwt *)

  let job_queue_is_empty m =
    let lwt =
      get_job_queue m
      >|= List.exists (fun _ -> true)
    in Lwt_main.run lwt

  let generate_task_queue operation map =
    keys map
    |> List.map (fun v -> (v, operation))
    |> (fun ops ->
        Logs.warn (fun m -> m "Generated task queue of [%s]"
                      (List.map (fun (a, b) -> Printf.sprintf "(%s, %s)" a b) ops
                       |> String.concat ", "));
         ops)
    |> fun ops -> Task_queue (ops, []) (* Initially there are no pending operations *)

  let set_task_queue q m =
    Store.set ~info:(Irmin_unix.info ~author:"map" "specifying workload")
      m ["task_queue"] q

  let push_job branch_name m = (* TODO: make this atomic *)
    get_job_queue m
    >>= fun js -> Store.set m ~info:(Irmin_unix.info ~author:"map" "Issuing map")
      ["job_queue"] (Job_queue (branch_name::js))

  (* TODO: make sure the right job is popped *)
  let pop_job m = (* TODO: make this atomic *)
    get_job_queue m
    >>= fun js -> match js with
    | (_::js) -> Store.set m ~info:(Irmin_unix.info ~author:"map" "Completed map")
                   ["job_queue"] (Job_queue js)
    | [] -> raise Empty_queue

  let map operation m =
    let lwt =

      (* TODO: ensure this name doesn't collide with existing branches *)
      let map_name = "map--" ^ Misc.generate_rand_string ~length:8 () in
      Logs.debug (fun m -> m "Map operation issued. Branch name %s" map_name);

      (* Push a map_request onto the master branch *)
      Store.set m
        ~info:(Irmin_unix.info ~author:"map" "Issuing map") ["map_request"] (Branch_name map_name)

      (* Create a new branch to isolate the operation *)
      >>= fun _ -> Store.of_branch (Store.repo m) map_name

      (* Generate and commit the task queue *)
      >>= fun branch -> Store.set
        ~info:(Irmin_unix.info ~author:"map" "specifying workload")
        branch ["task_queue"] (generate_task_queue branch operation)

      (* Wait for the task queue to be empty *)
      >|= (fun _ -> while not(get_task_queue_empty branch) do Unix.sleep 1 done)

      (* TODO: Merge the map branch into master *)
      >|= fun () -> ()

    in Lwt_main.run lwt; m

end
