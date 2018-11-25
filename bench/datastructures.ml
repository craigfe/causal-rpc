open Core
open Core_bench

type t = | Alice | Bob
type s = | A | B | C | D | E

let polymorphic_pattern () =
  let test v =
    match v with
    | `Alice   -> 100
    | `Bob     -> 101
    | `Charlie -> 102
    | `David   -> 103
    | `Eve     -> 104
  in
  List.iter [`Alice; `Bob; `Charlie; `David] ~f:(fun v -> ignore(test v))

let monomorphic_pattern_small () =
  let test v =
    match v with
    | Alice   -> 100
    | Bob     -> 101 in
  List.iter [ Alice; Bob ] ~f:(fun v -> ignore(test v))

let monomorphic_pattern_large () =
  let test v =
    match v with
    | A       -> 100
    | B       -> 101
    | C       -> 102
    | D       -> 103
    | E       -> 104
  in
  List.iter [ A; B; C; D ] ~f:(fun v -> ignore(test v))

let nrepeat func n () =
  for _ = 1 to n do func () done

let tests = [
  "Polymorphic_pattern",
  (fun len -> Staged.stage (nrepeat polymorphic_pattern len));

  "Monomorphic_larger_pattern",
  (fun len -> Staged.stage (nrepeat monomorphic_pattern_large len));

  "Monomorphic_small_pattern",
  (fun len -> Staged.stage (nrepeat monomorphic_pattern_small len));
]

let batch_sizes =
  let res = ref [] in
  let count = ref 1_000_000 in
  while !count >= 1  do
    res := (!count :: !res);
    count := !count / 2
  done;
  !res

let () =
  List.map tests ~f:(fun (name,test) -> Bench.Test.create_indexed ~name ~args:batch_sizes test)
  |> Bench.make_command
  |> Command.run
