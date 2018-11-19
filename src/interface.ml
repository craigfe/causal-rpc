type op = string * int32
let op = let open Irmin.Type in pair string int32

type param = string
let param = Irmin.Type.string

module Operation: Set.OrderedType with type t = op = struct
  type t = op
  let compare (a, _) (b, _) = String.compare a b
end
module OpSet = Set.Make(Operation)

module Implementation = struct
  type operation_key = string

  (* An implementation is a map from operations to type-preserving functions
     with string parameters *)
  type 'a t = (op, (param list -> 'a -> 'a)) Hashtbl.t

  let of_hashtable i = i

  let find_operation_opt key impl =
    Hashtbl.find_opt impl key
end

module Description = struct

  (* A description is a set of operations *)
  type t = OpSet.t

  let of_set s = s

  let valid_name name d =
    OpSet.exists (fun (n, _) -> name == n) d
end

module type DESC = sig
  type t
  val api: Description.t
end

module type IMPL = sig
  type t
  val api: t Implementation.t
end

exception Invalid_definition of string

module type S = sig
  val declare: string -> int32 -> op
  val describe: op list -> Description.t
  val implement: (op * (param list -> 'a -> 'a)) list -> 'a Implementation.t
end

module Make = struct

  let declare string opt = (string, opt)

  (* Simply convert the list to a set, return an exception if the list
     contains a duplicate *)
  let describe fns =
    let len = List.length fns in
    let set = OpSet.of_list fns in

    if (OpSet.cardinal set != len) then
      raise @@ Invalid_definition "Duplicate function name contained in list"
    else set

  (* Simply convert the list to a hashtable, return an exception if there
     are any duplicate function names *)
  let implement fns =
    let h = Hashtbl.create 10 in
    let rec aux fns = match fns with
      | [] -> h
      | (op, f)::fs -> match Hashtbl.find_opt h op with
        | Some _ -> raise @@ Invalid_definition ("Duplicate function name (" ^ (fst op) ^ ") in implementation")
        | None -> Hashtbl.add h op f; aux fs
    in Implementation.of_hashtable (aux fns)

end
