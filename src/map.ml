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
  val is_empty: t -> bool
  val mem: key -> t -> bool
  val add: key -> value -> t -> t
  val remove: key -> t -> t
  val size: t -> int
  val keys: t -> key list
  val values: t -> value list
  val map: t -> t
end

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

module Make(Eq : EqualityType) (Op: Operations with type t = Eq.t) = struct
  type key = string
  type value = Eq.t

  type t = Store.repo
  (* A map is a directory in an Irmin Key-value store *)

  let generate_random_directory () =
    Misc.generate_rand_string ~length:20 ()
    |> Pervasives.(^) "/tmp/irmin/set/"
    |> fun x -> Logs.info (fun m -> m "No directory supplied. Generated random directory %s" x); x

  let empty ?(directory=generate_random_directory()) () =
    let config = Irmin_git.config ~bare:true directory in

    (* Delete the directory if it already exists... Unsafe! *)
    let ret_code = Sys.command ("rm -rf " ^ directory) in begin
      if (ret_code <> 0) then invalid_arg "Unable to delete directory";

      Lwt_main.run (Store.Repo.v config)
    end

  let mem key m =
    let lwt =
      Store.master m
      >>= fun m -> Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.exists (fun (x,_) -> x = key)
    in Lwt_main.run lwt

  let add key value s =
    let str_val = Eq.to_string value in
    let lwt =
      Store.master s
      >>= fun master -> Store.set master
        ~info:(Irmin_unix.info ~author:"test" "Committing %s" str_val)
        ["vals"; key]
        str_val
    in Lwt_main.run lwt; s

  let remove _ _ = invalid_arg "TODO"

  let size s =
    let lwt =
      Store.master s
      >>= Store.tree
      >>= fun tree -> Store.Tree.list tree ["vals"]
      (* >|= List.filter (fun (_, typ) -> typ = `Contents) *)
      >|= List.length
    in Lwt_main.run lwt

  let is_empty s =
    (size s) == 0

  let keys s =
    let lwt =
      Store.master s
      >>= fun master -> Store.tree master
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.map(fst)
    in Lwt_main.run lwt

  let values s =
    let lwt =
      Store.master s
      >>= fun master -> Store.tree master
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >>= Lwt_list.map_p (fun (x, _) -> Store.get master ["vals"; x])
      >|= List.map Eq.of_string
    in Lwt_main.run lwt

  let map s =
    let lwt =
      Store.of_branch s "map--alpha"
      >>= fun branched_store -> Store.set branched_store
        ~info:(Irmin_unix.info ~author:"map" "%s" "Issuing map")
        ["map_request"]
        "map_contents"

    in Lwt_main.run lwt; s

  (* Push a map_request onto the master branch *)
  (* Create a new branch *)
  (* Wait for the request to become empty *)
  (* Return the result *)

    (* let ltw =
     *   Store.tree s
     *   >>= fun tree -> Store.Tree.list tree ["vals"]
     *     >>= Lwt_list.map_p (fun (x, _)) *)
end
