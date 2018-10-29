open Trace_rpc

let test_marshal () =
  let open Marshal in
  Rpc.make (Id.id_of_int 123) ["Some"; "example"; "params"] None
  |> Fmt.strf "%a\n" Rpc.pp
  |> Alcotest.(check string) "Simple serialisation" "A"

let print m = Fmt.pr "%a\n%!"  m

let test_base () =
  Alcotest.(check string) "The random name function begins with `server--`"
	  (String.sub (Server.generate_random_name()) 0 8) "server--"

let suite = [
	"type", [
	  "base", `Quick, test_base;
    "files", `Quick, test_marshal;
	]
]

let () = Alcotest.run "trace" suite
