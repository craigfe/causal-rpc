
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

type (_,_) func_type =
  | BaseType : ('a, 'a -> 'a) func_type
  | ParamType : (unit * ('a, 'b) func_type) -> ('a, (Param.t -> 'b)) func_type

type (_,_) func =
  | Base : ('a -> 'a) -> ('a, 'a -> 'a) func
  | Param : (Param.t -> ('a, 'b) func) -> ('a, (Param.t -> 'b)) func

module type OPERATION = sig
  type value
  type 'a unboxed
  type t = | B: 'a unboxed -> t
  type 'a matched_implementation = 'a unboxed * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi
  val name: 'a unboxed -> string
  val typ:  'a unboxed -> (value, 'a) func_type

  val return: ('a, 'a -> 'a) func_type
  val (-->): unit -> ('a, 'b) func_type -> ('a, Param.t -> 'b) func_type
  val declare: string -> (value, 'b) func_type -> 'b unboxed

  val compare: t -> t -> int
end

module Operation(T: Irmin.Contents.S): OPERATION with type value = T.t = struct
  type value = T.t

  type 'a unboxed = {
    name: string;
    typ: (value, 'a) func_type;
  }
  (* An operation is a function with a string name *)

  type t = | B: 'a unboxed -> t
  type 'a matched_implementation = 'a unboxed * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  let name {name = n; _} = n
  let typ {name = _; typ = t} = t

  let return = BaseType
  let (-->) p f = ParamType (p, f)

  let declare name typ = {name; typ}

  let compare a b =
    match a with
    | B {name = n1; _} -> match b with
      | B {name = n2; _} -> String.compare n1 n2
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




