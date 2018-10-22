open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

(* Commit information *)
let info author fmt = Irmin_unix.info ~author:author fmt

let generate_random_name () =
	Random.self_init (); (* Initialise the random number generator *)
    let rand_char_code () = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in

    let rand_string _ = rand_char_code ()
        |> char_of_int
        |> (String.make 1) in

    Array.init 8 rand_string
        |> Array.to_list
        |> List.cons "server--"
        |> String.concat ""
        |> fun x -> print_endline ("No server name supplied. Generated random name " ^ x ^ "."); x

let run ?(name=generate_random_name()) port =

    (* Open the repo *)
    Store.Repo.v config >>=

    (* Load the master branch *)
    Store.master >>= fun t ->

    (* Set key "foo/bar" to "testing 123" *)
    Store.set t ~info:(info name "Updating foo/bar") ["foo"; "bar"] (name ^ " running on " ^ (string_of_int port)) >>= fun () ->

    (* Get key "foo/bar" and print it to stdout *)
    Store.get t ["foo"; "bar"] >|=

    fun x -> Printf.printf "foo/bar => '%s'\n" x

let main name port = Lwt_main.run (run ?name port)
