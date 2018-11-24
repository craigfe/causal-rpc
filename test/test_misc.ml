open Trace_rpc

(** Simple tests of miscellaneous features of the library *)
let test () =
  Alcotest.(check string) "The random name function begins with `server--`"
	  (String.sub (Server.random_server_name()) 0 8) "server--"
