open Type

type (_,_) func_type =
  | BaseType : ('a, 'a -> 'a) func_type
  | ParamType : (unit * ('a, 'b) func_type) -> ('a, (Param.t -> 'b)) func_type

type (_,_) func =
  | Base : ('a -> 'a) -> ('a, 'a -> 'a) func
  | Param : (Param.t -> ('a, 'b) func) -> ('a, (Param.t -> 'b)) func

type (_,_) params_gadt =
  | V : ('v, 'v -> 'v) params_gadt
  | P : (Param.t * ('v,'a) params_gadt) -> ('v, Param.t -> 'a) params_gadt

module type OPERATION = sig
  module S: Irmin.Contents.S

  type 'a unboxed
  type 'a params = (S.t, 'a) params_gadt
  type t = | B: 'a unboxed -> t
  type 'a matched_implementation = 'a unboxed * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi
  val name: 'a unboxed -> string
  val typ:  'a unboxed -> (S.t, 'a) func_type

  val return: ('a, 'a -> 'a) func_type
  val (-->): unit -> ('a, 'b) func_type -> ('a, Param.t -> 'b) func_type
  val declare: string -> (S.t, 'b) func_type -> 'b unboxed

  val compare: t -> t -> int
end

module Operation(T: Irmin.Contents.S): OPERATION with module S = T = struct
  module S = T
  type value = S.t

  type 'a unboxed = {
    name: string;
    typ: (value, 'a) func_type;
  }
  (* An operation is a function with a string name *)

  type 'a params = (value, 'a) params_gadt
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
module Description(T: Irmin.Contents.S) = struct
  module S = T
  module Op = Operation(T)
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
        | Op.B unboxed -> (Op.name unboxed) == name) d
end

module type IMPL_MAKER = sig
  module S: Irmin.Contents.S
  module Op: OPERATION with module S = S

  type t
  (** The type of implementations of functions from type 'a to 'a *)

  val implement: 'a Op.unboxed -> 'a -> Op.boxed_mi

  val define: Op.boxed_mi list -> t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> t -> Op.boxed_mi option
  (** Retreive an operation from an implementation *)
end

module MakeImplementation(T: Irmin.Contents.S): IMPL_MAKER
  with module S = T
   and module Op = Operation(T) = struct
  module S = T
  module Op = Operation(T)

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
          let n = Op.name unboxed in

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
  module S: Irmin.Contents.S
  val api: Description(S).t
end


module type IMPL = sig
  module S: Irmin.Contents.S
  val api: MakeImplementation(S).t
end




