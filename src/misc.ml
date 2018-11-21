let generate_rand_string ?(length=8) () =
	Random.self_init (); (* Initialise the random number generator *)

  let rand_char_code () = match Random.int(26+26+10) with
      n when n < 26      -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n                  -> int_of_char '0' + n - 26 - 26 in

  let rand_string _ = rand_char_code ()
                      |> char_of_int
                      |> (String.make 1) in

  Array.init length rand_string
	|> Array.to_list
	|> String.concat ""

let timestamp () =
  let ts = Unix.gettimeofday() in
  let tm = Unix.localtime ts in
  let us, _s = modf ts in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%03d "
    (1900 + tm.Unix.tm_year)
    (1    + tm.Unix.tm_mon)
    (tm.Unix.tm_mday)
    (tm.Unix.tm_hour)
    (tm.Unix.tm_min)
    (tm.Unix.tm_sec)
    (int_of_float (1_000. *. us))
