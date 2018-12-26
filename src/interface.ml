type (_,_) func_type =
  | BaseType : ('a, 'a -> 'a) func_type
  | ParamType : ('t Type.t * ('a, 'b) func_type) -> ('a, ('t -> 'b)) func_type

type (_,_) params_gadt =
  | Unit : ('v, 'v -> 'v) params_gadt
  | Param : ('p Type.t * 'p * ('v,'a) params_gadt) -> ('v, 'p -> 'a) params_gadt

module type OPERATION = sig
  module Val: Irmin.Contents.S

  module Unboxed: sig
    type 'a t

    val name: 'a t -> string
    val typ:  'a t -> (Val.t, 'a) func_type
  end

  type 'a params = (Val.t, 'a) params_gadt
  type t = | B: 'a Unboxed.t -> t
  type 'a matched_implementation = 'a Unboxed.t * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  val return: ('a, 'a -> 'a) func_type
  val (@->): 'p Type.t -> ('a, 'b) func_type -> ('a, 'p -> 'b) func_type
  val declare: string -> (Val.t, 'b) func_type -> 'b Unboxed.t

  val compare: t -> t -> int
end

module MakeOperation(T: Irmin.Contents.S): OPERATION with module Val = T = struct
  module Val = T

  module Unboxed = struct
    type 'a t = {
      name: string;
      typ: (Val.t, 'a) func_type;
    }

    let name {name = n; _} = n
    let typ {name = _; typ = t} = t
  end

  type 'a params = (Val.t, 'a) params_gadt
  type t = | B: 'a Unboxed.t -> t
  type 'a matched_implementation = 'a Unboxed.t * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi


  let return = BaseType
  let (@->) p f = ParamType (p, f)

  let declare name typ: 'a Unboxed.t = {Unboxed.name = name; Unboxed.typ = typ}

  let compare a b =
    match (a, b) with
    | B {Unboxed.name = n1; _}, B {Unboxed.name = n2; _} ->
      String.compare n1 n2
end


exception Invalid_description of string
module Description(Val: Irmin.Contents.S) = struct
  module Op = MakeOperation(Val)
  module OpSet = Set.Make(Op)

  (* A description is a set of operations *)
  type t = OpSet.t

  let describe unboxed = Op.B unboxed

  (* Simply convert the list to a set, return an exception if the list
     contains a duplicate *)
  let define fns =
    let len = List.length fns in
    let set = OpSet.of_list fns in

    if (OpSet.cardinal set != len) then
      raise @@ Invalid_description "Duplicate function name contained in list"
    else set

  let valid_name name d =
    OpSet.exists (fun b -> match b with
        | Op.B unboxed -> (Op.Unboxed.name unboxed) == name) d
end

module type IMPL_MAKER = sig
  module S: Irmin.Contents.S
  module Op: OPERATION with module Val = S

  type t
  (** The type of implementations of functions from type 'a to 'a *)

  val implement: 'a Op.Unboxed.t -> 'a -> Op.boxed_mi

  val define: Op.boxed_mi list -> t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> t -> Op.boxed_mi option
  (** Retreive an operation from an implementation *)
end

module MakeImplementation(T: Irmin.Contents.S): IMPL_MAKER
  with module S = T
   and module Op = MakeOperation(T) = struct
  module S = T
  module Op = MakeOperation(T)

  (* An implementation is a map from operations to type-preserving functions
     with string parameters *)
  type t = (string, Op.boxed_mi) Hashtbl.t

  let implement unboxed func = Op.E (unboxed, func)

  (* Simply convert the list to a hashtable, return an exception if there
     are any duplicate function names *)
  let define fns =
    let h = Hashtbl.create 10 in
    let rec aux fns = match fns with
      | [] -> h
      | f::fs -> match f with
        | Op.E (unboxed, _) ->
          let n = Op.Unboxed.name unboxed in

          match Hashtbl.find_opt h n with

          | Some _ -> raise @@ Invalid_description
              ("Duplicate function name (" ^ n ^ ") in implementation")

          (* This name has not been used before *)
          | None -> Hashtbl.add h n f; aux fs
    in aux fns

  let find_operation_opt key impl =
    Hashtbl.find_opt impl key
end


module type DESC = sig
  module Val: Irmin.Contents.S
  val api: Description(Val).t
end


module type IMPL = sig
  module Val: Irmin.Contents.S
  val api: MakeImplementation(Val).t
end




