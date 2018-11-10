module SS = Set.Make(String)

module Implementation = struct
  type operation_key = string

  (* An implementation is a map from function names to type-preserving functions *)
  type 'a t = (string, ('a -> 'a)) Hashtbl.t

  let of_hashtable i = i

  let find_operation_opt key impl =
    Hashtbl.find_opt impl key
end

module Description = struct
  type op = string

  (* A description is a set of function names *)
  type t = SS.t

  let of_set s = s

  let valid_operation op desc =
    SS.mem desc op
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
  val declare: string list -> Description.t
  val implement: (string * ('a -> 'a)) list -> 'a Implementation.t
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
