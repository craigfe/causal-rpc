open Lwt.Infix

module type Operations = sig
  type t
  val iter: t -> t
end

type 'v contents =
  | Value of 'v
  | Branch_name of string

module MakeContents (Val: Irmin.Contents.S) : Irmin.Contents.S
  with type t = Val.t contents = struct

  type t = Val.t contents

  let t =
    let open Irmin.Type in
    variant "contents" (fun value branch_name -> function
        | Value v -> value v
        | Branch_name n -> branch_name n)
    |~ case1 "Value" Val.t (fun v -> Value v)
    |~ case1 "Branch_name" string (fun s -> Branch_name s)
    |> sealv

  let pp = Irmin.Type.pp_json t

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    Irmin.Type.decode_json t decoder

  let merge = Irmin.Merge.(option (idempotent t))
end

module type S = sig
  type key = string
  type value
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
  val map: t -> t
end

module Make (Val : Irmin.Contents.S) (Op: Operations with type t = Val.t) = struct
  module Contents = MakeContents(Val)
  module Store = Irmin_unix.Git.FS.KV(Contents)
  module Sync = Irmin.Sync(Store)

  type key = string
  type value = Val.t

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

  let map m =
    let lwt =

      (* TODO: ensure this name doesn't collide with existing branches *)
      let map_name = "map--" ^ Misc.generate_rand_string ~length:8 () in
      Logs.debug (fun m -> m "Map operation issued. Branch name %s" map_name);

      (* Push a map_request onto the master branch *)
      Store.set m
        ~info:(Irmin_unix.info ~author:"map" "Issuing map") ["map_request"] (Branch_name map_name)

      (* Create a new branch to isolate the operation *)
      >>= fun _ -> Store.of_branch (Store.repo m) map_name
      >>= fun branch -> Store.set ~info:(Irmin_unix.info ~author:"map" "specifying workload") branch ["todo"] (Branch_name "true")

      (* Wait for todo to be set to false *)
      >|= (fun _ -> while Lwt_main.run(Store.get branch ["todo"]) = (Branch_name "true") do Unix.sleep 1 done)

      (* TODO: Merge the map branch into master *)
      >|= fun () -> ()

    in Lwt_main.run lwt; m

  (* Wait for the request to become empty *)
  (* Return the result *)

    (* let ltw =
     *   Store.tree s
     *   >>= fun tree -> Store.Tree.list tree ["vals"]
     *     >>= Lwt_list.map_p (fun (x, _)) *)
end
