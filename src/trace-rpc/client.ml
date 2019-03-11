open Lwt.Infix

(* A client receives requests on a single key, and pushes onto the job_queue of
   a remote server*)
module type S = sig
  module Store: Store.S
  module Value = Store.Value

  type t
  (** A client *)

  val empty: ?directory:string -> remote_uri:string -> name:string -> t Lwt.t

  val rpc: ?timeout:float
    -> (Value.t,'a,'p) Interface.NamedOp.t
    -> (Value.t, 'a) Interface.params
    -> t -> t Lwt.t
end

module Make (Store: Store.S): S with module Store = Store = struct
  module Store = Store
  module Value = Store.Value

  (* Private modules *)
  module IStore = Store.IrminStore
  module ISync = Store.IrminSync

  type t = {
    local: IStore.t;
    remote: Irmin.remote;
    name: IStore.branch (* The name of the branch that belongs to us *)
  }

  let generate_random_directory () =
    Misc.generate_rand_string ~length:20 ()
    |> Pervasives.(^) "/tmp/irmin/client/"
    |> fun x -> Logs.info (fun m -> m "No directory supplied. Generated random directory %s" x); x

  let empty ?(directory=generate_random_directory()) ~remote_uri ~name =
    let config = Irmin_git.config ~bare:true directory in

    (* Delete the directory if it already exists... Unsafe! *)
    let ret_code = Sys.command ("rm -rf " ^ directory) in
    if (ret_code <> 0) then invalid_arg "Unable to delete directory";

    IStore.Repo.v config
    >>= fun repo -> IStore.of_branch repo name
    >>= fun local -> Store.upstream remote_uri name
    >|= fun remote -> {local; remote; name}

  let rpc ?(timeout=5.0) operation params m =
    invalid_arg "not implemented"
end

