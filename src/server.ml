open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

(* Commit information *)
let info author fmt = Irmin_unix.info ~author:author fmt

let generate_random () = "rand" (* TODO: implement *)

let run ?(name=generate_random()) port =

    (* Open the repo *)
    Store.Repo.v config >>=

    (* Load the master branch *)
    Store.master >>= fun t ->

    (* Set key "foo/bar" to "testing 123" *)
    Store.set t ~info:(info name "Updating foo/bar") ["foo"; "bar"] (string_of_int port) >>= fun () ->

    (* Get key "foo/bar" and print it to stdout *)
    Store.get t ["foo"; "bar"] >|=

    fun x -> Printf.printf "foo/bar => '%s'\n" x

let main name port = Lwt_main.run (run ?name port)