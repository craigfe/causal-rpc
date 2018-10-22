let test_base () =
    Alcotest.(check string) "The random name function begins with `server--`"
	    (String.sub (Trace.Server.generate_random_name()) 0 8) "server--"

let suite = [
	"type", [
		"base", `Quick, test_base;
	]
]

let () = Alcotest.run "trace" suite
