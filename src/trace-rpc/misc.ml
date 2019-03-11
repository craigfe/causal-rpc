module IntSet = Set.Make(struct type t = int let compare = Pervasives.compare end)

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

(* Convert two lists to a list of pairs *)
let zip list1 list2 =
  let rec inner l1 l2 acc = match l1, l2 with
    | [], [] -> acc
    | x::xs, y::ys -> inner xs ys ((x, y)::acc)
    | _ -> invalid_arg "Lists do not have the same length"
  in inner list1 list2 []

(* Take the first [n] items from a list [l] and return a pair of the first
   [n] items and the remaining items, all in the original order. *)
let split_sequential n l =
  if n < 0 then invalid_arg "Cannot take a negative number of items from a list";

  let rec inner n list acc = match n, list with
    | 0, t -> (List.rev acc, t)
    | _, [] -> invalid_arg "Attempted to take too many items from the list"
    | n, t::ts -> inner (n-1) ts (t::acc)
  in inner n l []

(* Take [n] items randomly from a list [l] and return a pair of the selected
   items and the remaining items, all in the original order. *)
let split_random n l =
  if n < 0 then invalid_arg "Cannot take a negative number of items from a list";

  let is =
    (* if n = 1, generate the index of the element to select *)
    if n = 1 then [Random.int (List.length l)]

    (* otherwise, generate random numbers until we have n of them *)
    else
      let s = ref IntSet.empty in
      while IntSet.cardinal !s < n do
        let i = Random.int (List.length l) in
        s := IntSet.add i !s
      done;

      List.sort Pervasives.compare (IntSet.elements !s)
  in


  let rec inner i is list (l_acc, r_acc) = match is, list with
    | []   , xs               -> (List.rev l_acc, (List.rev r_acc) @ xs)
    | _    , []               -> invalid_arg "Attempted to take too many items from the list"
    | c::is, x::xs when c = i -> inner (i+1) is xs (x::l_acc, r_acc)
    | is, x::xs               -> inner (i+1) is xs (l_acc, x::r_acc)
  in
  inner 0 is l ([], [])

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

let check_within_tmp dir =
  if String.sub dir 0 11 <> "/tmp/irmin/"
  then invalid_arg ("Supplied directory (" ^ dir ^ ") must be in /tmp/irmin/");
