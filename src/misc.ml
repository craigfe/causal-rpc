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

let pick_random p =
  let i = Random.int (List.length p) in

  let rec inner n list acc = match n, list with
    | 0, x::xs -> (x, (List.rev acc) @ xs)
    | n, x::xs -> inner (n-1) xs (x::acc)
    | _, [] -> invalid_arg "Misc.pick_random internal error"

  in inner i p []


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

let pp_level ppf = function
  | Logs.App -> Format.pp_print_string ppf "APP"
  | Logs.Error -> Format.pp_print_string ppf "ERROR"
  | Logs.Warning -> Format.pp_print_string ppf "WARNING"
  | Logs.Info -> Format.pp_print_string ppf "INFO"
  | Logs.Debug -> Format.pp_print_string ppf "DEBUG"

let handle_merge_conflict src_branch dst_branch merge_result = match merge_result with
  | Ok () -> Lwt.return_unit
  | Error `Conflict c ->
    Lwt.fail_with (Printf.sprintf "Conflict when attempting merge from %s into %s: %s" src_branch dst_branch c)

let pp_exec_header =
  let x = match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | _ -> Filename.basename Sys.argv.(0) in
  let pf = Format.fprintf in

  fun ppf (l, (s:Logs.src), h) -> match h with
    | None -> pf ppf "%s (%s): [%a] " x (Logs.Src.name s) pp_level l
    | Some h -> pf ppf ("%s (%s): [%s] ") x (Logs.Src.name s) h

let reporter ppf =
  let report (src:Logs.src) level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags:_ fmt ->
    Format.kfprintf k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_exec_header (level, src, header)
  in
  { Logs.report = report }

let set_reporter () =
  Logs.set_reporter (reporter Format.std_formatter)
