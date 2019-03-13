open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
(* open Yojson *)

exception Http_error of int

let get_body url =
  Client.get (Uri.of_string url)
  >>= fun (resp, body) ->

  let code = resp |> Response.status |> Code.code_of_status in
  match code with
  | 200 -> Cohttp_lwt.Body.to_string body
  | c -> Lwt.fail @@ Http_error c

let get_repos =
    get_body "https://api.github.com/orgs/mirage/repos"
    >|= Yojson.Basic.from_string
    >|= Yojson.Basic.Util.(convert_each (fun j -> ((to_string (member "name" j)), to_string (member "url" j))))

let commit_count url =
  let lwt =
    Trace_rpc.Helpers.set_reporter ();
    Logs.set_level (Some Logs.Info);
    Logs.warn (fun m -> m "Attempting request at URI %s/commits " url);

    Lwt.catch (fun () ->
        get_body (url ^ "/commits")
        >|= Yojson.Basic.from_string
        >|= Yojson.Basic.Util.(convert_each to_assoc)
        >|= List.length)

      (* We catch the exception here, until we have a proper way of dealing with it *)
      (fun exn -> match exn with
         | Http_error n -> Lwt.return (-n)
         | e -> raise e)

  in Lwt_main.run lwt
