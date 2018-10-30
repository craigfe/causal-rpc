open Trace_rpc

let test_marshal () =
  let open Marshal in begin
    let rpc_test_case (description, rpc, expected_value) =
      rpc
      |> Fmt.strf "%a" Rpc.pp
      |> Alcotest.(check string) description expected_value

    in let test_cases =
         [ ( "Simple serialisation without results"
           , Rpc.make (Id.id_of_int 123) ["Some"; "example"; "params"] None
           , "{\"id\":123,\"params\":[\"Some\",\"example\",\"params\"]}")

         ; ( "Simple serialisation with results"
           , Rpc.make (Id.id_of_int 0) [] (Some "result in here")
           , "{\"id\":0,\"params\":[],\"result\":\"result in here\"}")
         ]

    in List.iter rpc_test_case test_cases
  end

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
