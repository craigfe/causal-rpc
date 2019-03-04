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

  type _ interface =
    | Unary : 'a Unboxed.t -> 'a interface
    | Complex : ('a Unboxed.t * 'b interface) -> ('a * 'b) interface

  type 'a implementation = 'a interface * 'a

  type 'a params = (Val.t, 'a) params_gadt
  type t = | B: 'a Unboxed.t -> t
  type 'a matched_implementation = 'a Unboxed.t * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  val return: ('a, 'a -> 'a) func_type
  val (@->): 'p Type.t -> ('a, 'b) func_type -> ('a, 'p -> 'b) func_type
  val declare: string -> (Val.t, 'b) func_type -> 'b interface

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

  type _ interface =
    | Unary : 'a Unboxed.t -> 'a interface
    | Complex : ('a Unboxed.t * 'b interface) -> ('a * 'b) interface

  type 'a implementation = 'a interface * 'a

  type 'a params = (Val.t, 'a) params_gadt
  type t = | B: 'a Unboxed.t -> t
  type 'a matched_implementation = 'a Unboxed.t * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi


  let return = BaseType
  let (@->) p f = ParamType (p, f)

  let declare name typ: 'a interface =
    Unary {Unboxed.name = name; Unboxed.typ = typ}

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
  type 'i t = OpSet.t

  let describe unboxed = Op.B unboxed

  let rec interface_to_list: type i. i Op.interface -> Op.t list = fun interface ->
    match interface with
    | Op.Unary t -> [Op.B t]
    | Op.Complex (t, ts) -> (Op.B t)::interface_to_list(ts)

  let (@): 'a Op.interface -> 'b Op.interface -> ('a * 'b) Op.interface = fun i is ->
      match i with
      | Op.Unary t -> Op.Complex (t, is)
      | _ -> invalid_arg "Cannot compose implementations like this"

  let define: 'i Op.interface -> 'i t = fun interface ->
    let l = interface_to_list interface in
    let len = List.length l in
    let set = OpSet.of_list l in

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

  type 'i t
  (** The type of implementations with type structure 'i from type 'a to 'a *)

  val (@): 'a Op.implementation -> 'b Op.implementation -> ('a * 'b) Op.implementation

  val define: 'i Op.implementation -> 'i t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> 'i t -> Op.boxed_mi option
  (** Retreive an operation from an implementation *)
end

module MakeImplementation(T: Irmin.Contents.S): IMPL_MAKER
  with module S = T
   and module Op = MakeOperation(T) = struct
  module S = T
  module Op = MakeOperation(T)

  (* An implementation is a map from operations to type-preserving functions
     with string parameters *)
  type 'i t = (string, Op.boxed_mi) Hashtbl.t

  (* Combine two implementations by aggregating the prototypes and storing the
     functions as nested pairs. We require that the first implementation contains only a
     single operation. *)
  let (@): 'a Op.implementation -> 'b Op.implementation -> ('a * 'b) Op.implementation
    = fun (proto_interface, func) (acc_interface, acc_functions) ->

      match proto_interface with
    | Op.Unary proto -> (Op.Complex (proto, acc_interface), (func, acc_functions))
    | _ -> invalid_arg "Cannot compose implementations like this"

  (* Helper function to add a type declaration and function to a hashtable *)
  let add_to_hashtable h typ func =
    let n = Op.Unboxed.name typ in (* the name of the function *)
    let boxed = Op.E (typ, func) in (* the format we store in the hashmap *)

    (match Hashtbl.find_opt h n with

     | Some _ -> raise @@ Invalid_description
         ("Duplicate function name (" ^ n ^ ") in implementation")

     (* This name has not been used before *)
     | None -> Hashtbl.add h n boxed)

  (* Simply convert the list to a hashtable, return an exception if there
     are any duplicate function names *)
  let define: 'i Op.implementation -> 'i t = fun fns ->

    let h = Hashtbl.create 10 in

    let rec aux: type a. a Op.implementation -> unit = fun impl -> match impl with
      | (Op.Unary t, f) -> add_to_hashtable h t f
      | (Op.Complex (t, ts), (f, fs)) -> add_to_hashtable h t f; aux (ts, fs)

    in aux fns; h

  let find_operation_opt key impl =
    Hashtbl.find_opt impl key
end


module type DESC = sig
  module Val: Irmin.Contents.S
  type shape
  val api: shape Description(Val).t
end


module type IMPL = sig
  module Val: Irmin.Contents.S
  type shape
  val api: shape MakeImplementation(Val).t
end




