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
  type elt
  type t

  val empty: ?directory:string -> unit -> t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val size: t -> int
  val elements: t -> elt list

  (* TODO: generalise this to one of a set of functions *)
  val map: t -> t
end

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

module Make(Eq : EqualityType) (Op: Operations with type t = Eq.t) = struct
  type elt = Eq.t
  type t = Store.repo
  (* A set is a directory in an Irmin Key-value store *)

  let last_key = ref 0
  let generate_new_key () =
    last_key := !last_key + 1;
    string_of_int !last_key

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

  let mem element s =
    let str_val = Eq.to_string element in
    let lwt =
      Store.master s
      >>= fun m -> Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >>= Lwt_list.map_p (fun (x, _) -> Store.get m ["vals"; x])
      >|= List.exists (fun x -> x = str_val)
    in Lwt_main.run lwt

  let add element s =
    let str_val = Eq.to_string element in
    let lwt =
      Store.master s
      >>= fun master -> Store.set master
        ~info:(Irmin_unix.info ~author:"test" "Committing %s" str_val)
        ["vals"; generate_new_key ()]
        str_val
    in Lwt_main.run lwt; s

  let remove _ _ = invalid_arg "TODO"

  let size s =
    let lwt =
      Store.master s
      >>= Store.tree
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.filter (fun (_, typ) -> typ = `Contents)
      >|= List.length
    in Lwt_main.run lwt

  let is_empty s =
    (size s) == 0

  let elements s =
    let lwt =
      Store.master s
      >>= fun master -> Store.tree master
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >>= Lwt_list.map_p (fun (x, _) ->
          Store.get master ["vals"; x])
      >|= List.map Eq.of_string
    in Lwt_main.run lwt
end
