module SS = Set.Make(String)

(* An implementation is a map from function names to type-preserving functions *)
type 'a implementation = (string, ('a -> 'a))  Hashtbl.t

(* A description is simply a set of function names *)
type description = SS.t

module type DESC = sig
  type t
  val api: description
end

module type IMPL = sig
  type t
  val api: t implementation
end

exception Invalid_definition of string

module type S = sig
  val declare: string list -> description
  val implement: (string * ('a -> 'a)) list -> 'a implementation
end

module Make = struct

  (* Simply convert the list to a set, return an exception if the list
     contains a duplicate *)
  let declare fns =
    let len = List.length fns in
    let set = SS.of_list fns in

    if (SS.cardinal set != len) then
      raise @@ Invalid_definition "Duplicate function name contained in list"
    else set

  (* Simply convert the list to a hashtable, return an exception if there
     are any duplicate function names *)
  let implement fns =
    let h = Hashtbl.create 10 in
    let rec aux fns = match fns with
      | [] -> h
      | (name, f)::fs -> match Hashtbl.find_opt h name with
        | Some _ -> raise @@ Invalid_definition ("Duplicate function name (" ^ name ^ ") in implementation")
        | None -> Hashtbl.add h name f; aux fs
    in aux fns

end
