
type z = Zer
type 'a s = Suc of 'a

type _ nat =
  | Zero : z nat
  | Succ : 'n nat -> 'n s nat

let rec box_nat: type n. n nat -> int = function
  | Zero -> 0
  | Succ n -> 1 + box_nat n

let rec unbox_nat: int -> 'n nat = fun n -> match n with
  | 0 -> Zero
  | n -> let p = unbox_nat (n-1) in Succ p

type 'n op = string * 'n nat

type boxed_op = string * int32

let rec box_op op = match op with
  | Unit (name, _) -> (name, Int32.zero)
  | Function (name, arity) -> (n)
type param = string

let op = let open Irmin.Type in pair string int32
let param = Irmin.Type.string

open Llist

type (_, _) func =
  | Function : param * ('n, 'b) func -> ('n s, param -> 'b) func
  | Returning : param -> (z, param) func
type 'a boxed_func = B: ('n, 'a) func -> 'a boxed_func

type ('n, 'a) implementation = 'n op * ('n, 'a) func
type 'a boxed_implementation = B: 'n op * ('n, 'a) func -> 'a boxed_implementation

module Implementation = struct

  (* An implementation is a map from operations to type-preserving functions
     with string parameters *)

  type 'a t = ('a boxed_func, 'a boxed_implementation) Hashtbl.t
  (* type _ t = | N: ('n, (('n op, ('n, 'a) func) Hashtbl.t)) Hashtbl.t t *)

  let find_operation_opt op impl =
    let arity = snd op in
    match Hashtbl.find_opt impl arity with
    | Some fn_hashtbl -> Hashtbl.find_opt fn_hashtbl op
    | None -> None

  let seal impl_list =
    let h = Hashtbl.create 10 in
    let rec aux impls = match impls with
      | [] -> h
      | (i::is) -> match i with
        | B (op, func) -> Hashtbl.add h op func; aux is

    in aux impl_list
end

module Description = struct
  (* A description is a set of operations *)
  type t = OpSet.t

  let of_set s = s

  let valid_name name d =
    OpSet.exists (fun n -> name == n) d
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
  val declare: string -> int32 -> z op
  val describe: z op list -> Description.t
  val implement: ('n op * ((param, 'n) Llist.t -> 'a -> 'a)) list
    -> 'a Implementation.t
end

module Make = struct


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
