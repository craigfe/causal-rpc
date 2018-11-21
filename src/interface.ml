
module Param = struct
  type t =
    | Unit of unit
    | Bool of bool
    | Char of char
    | Int32 of int32
    | Int64 of int64
    | String of string

  let irmin_t = let open Irmin.Type in
    variant "irmin_t" (fun unit bool char int32 int64 string -> function
        | Unit u -> unit u
        | Bool b -> bool b
        | Char c -> char c
        | Int32 i -> int32 i
        | Int64 i -> int64 i
        | String s -> string s
      )
  |~ case1 "Unit" Irmin.Type.unit (fun u -> Unit u)
  |~ case1 "Bool" Irmin.Type.bool (fun b -> Bool b)
  |~ case1 "Char" Irmin.Type.char (fun c -> Char c)
  |~ case1 "Int32" Irmin.Type.int32 (fun i -> Int32 i)
  |~ case1 "Int64" Irmin.Type.int64 (fun i -> Int64 i)
  |~ case1 "String" Irmin.Type.string (fun u -> String u)
  |> sealv
end

module Operation = struct
  type t = string * int32
  let t = let open Irmin.Type in pair string int32

  let test_t = Alcotest.(pair string int32)

  let name (n, _) = n
  let arity (_, a) = Int32.to_int a

  let declare name arity = (name, Int32.of_int arity)
  let compare (a, _) (b, _) = String.compare a b
end

exception Invalid_description of string
module Description = struct

  module OpSet = Set.Make(Operation)

  (* A description is a set of operations *)
  type 'a t = OpSet.t

  (* Simply convert the list to a set, return an exception if the list
     contains a duplicate *)
  let define fns =
    let len = List.length fns in
    let set = OpSet.of_list fns in

    if (OpSet.cardinal set != len) then
      raise @@ Invalid_description "Duplicate function name contained in list"
    else set

  let valid_name name d =
    OpSet.exists (fun (n, _) -> name == n) d
end


module Implementation = struct
  type operation_key = string

  (* An implementation is a map from operations to type-preserving functions
     with string parameters *)
  type 'a t = (Operation.t, (Param.t list -> 'a -> 'a)) Hashtbl.t

  (* Simply convert the list to a hashtable, return an exception if there
     are any duplicate function names *)
  let implement fns =
    let h = Hashtbl.create 10 in
    let rec aux fns = match fns with
      | [] -> h
      | (op, f)::fs -> match Hashtbl.find_opt h op with
        | Some _ -> raise @@ Invalid_description ("Duplicate function name (" ^ (fst op) ^ ") in implementation")
        | None -> Hashtbl.add h op f; aux fs
    in aux fns

  let find_operation_opt key impl =
    Hashtbl.find_opt impl key
end


module type DESC = sig
  type t
  val api: t Description.t
end


module type IMPL = sig
  type t
  val api: t Implementation.t
end




