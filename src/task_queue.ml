
type task = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
} [@@deriving show]

let task =
  let open Irmin.Type in
  record "task" (fun name params key -> { name; params; key })
  |+ field "name" string (fun t -> t.name)
  |+ field "params" (list Type.Boxed.irmin_t) (fun t -> t.params)
  |+ field "key" string (fun t -> t.key)
  |> sealr

type t = (task list * task list) [@@deriving show]
let t = Irmin.Type.(pair (list task) (list task))

type subset_relation = Subset | Superset | Noninclusive

(* prefix_list x y is Subset if x is a prefix of y, Superset if y is a strict prefix of y,
   or Noninclusive if neither is a prefix of the other *)
let prefix_list x y =
  let rec inner a b = match (a, b) with
    | [], _ -> Subset
    | _, [] -> Superset
    | (x::xs), (y::ys) when x = y -> inner xs ys
    | _ -> Noninclusive
  in inner x y

(* suffix_list x y is Subset if x is a suffix of y, Superset if y is a strict suffix of y,
   or Noninclusive if neither is a suffix of the other *)
let suffix_list a b =
  prefix_list (List.rev a) (List.rev b)

let merge ~old:_ x y =
  let (todo_x, todo_y) = (fst x, fst y) in

  (* If one todo list is the subset of another, retain the subset *)
  match suffix_list todo_x todo_y with
  | Subset -> Irmin.Merge.ok x
  | Superset -> Irmin.Merge.ok y
  | Noninclusive -> Irmin.Merge.conflict "Task queues could not be merged"
