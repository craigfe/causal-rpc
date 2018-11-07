open Lwt.Infix

module type EqualityType = sig
  type t

  val (=): t -> t -> bool
  val of_string: string -> t
  val to_string: t -> string
end

module type Operations = sig
  type t
  val iter: t -> t
end

module type S = sig
  type key = string
  type value
  type t

  val empty: ?directory:string -> unit -> t
  val of_store: Irmin_unix.Git.FS.KV(Irmin.Contents.String).t -> t
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

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

module Make (Eq : EqualityType) (Op: Operations with type t = Eq.t) = struct
  type key = string
  type value = Eq.t

  type t = Store.t
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
    let str_val = Eq.to_string value in
    let lwt =
      Store.set m
        ~info:(Irmin_unix.info ~author:"test" "Committing %s" str_val)
        ["vals"; key]
        str_val
    in Lwt_main.run lwt; m

  let find key m =
    let lwt =
        (* Get the value from the store and deserialise it *)
      Store.get m ["vals"; key]
      >|= Eq.of_string

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
      >|= List.map Eq.of_string
    in Lwt_main.run lwt

  let map m =
    let lwt =

      (* TODO: ensure this name doesn't collide with existing branches *)
      let map_name = "map--" ^ Misc.generate_rand_string ~length:8 () in
      Logs.debug (fun m -> m "Map operation issued. Branch name %s" map_name);


      (* Push a map_request onto the master branch *)
      Store.set m
        ~info:(Irmin_unix.info ~author:"map" "Issuing map") ["map_request"] map_name

      (* Create a new branch to isolate the operation *)
      >>= fun _ -> Store.of_branch (Store.repo m) map_name
      >>= fun branch -> Store.set ~info:(Irmin_unix.info ~author:"map" "specifying workload") branch ["todo"] "true"

      (* Wait for todo to be set to false *)
      >|= (fun _ -> while Lwt_main.run(Store.get branch ["todo"]) = "true" do Unix.sleep 1 done)

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
