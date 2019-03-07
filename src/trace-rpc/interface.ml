type (_,_) prototype =
  | BaseType : ('a, 'a -> 'a) prototype
  | ParamType : ('t Type.t * ('a, 'b) prototype) -> ('a, ('t -> 'b)) prototype

type (_,_) params =
  | Unit : ('v, 'v -> 'v) params
  | Param : ('p Type.t * 'p * ('v,'a) params) -> ('v, 'p -> 'a) params

module NamedOp = struct
  type ('v, 'a) t = {
    name: string;
    typ: ('v, 'a) prototype;
  }

  let name {name = n; _} = n
  let typ {name = _; typ = t} = t
end

type (_,_) interface =
  | Unary : ('v,'a) NamedOp.t -> ('v,'a) interface
  | Complex : (('v,'a) NamedOp.t * ('v, 'b) interface) -> ('v, 'a * 'b) interface

type ('v,'a) implementation = ('v,'a) interface * 'a

module type OPERATION = sig
  module Val: Irmin.Contents.S


  type t = | B: (Val.t, 'a) NamedOp.t -> t
  type 'a matched_implementation = (Val.t, 'a) NamedOp.t * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  (* val apply: ('v,'a) interface -> 'a -> (('v,'a) interface * ('v, 'a) params) *)
  val return: ('a, 'a -> 'a) prototype
  val (@->): 'p Type.t -> ('a, 'b) prototype -> ('a, 'p -> 'b) prototype
  val declare: string -> (Val.t, 'b) prototype -> (Val.t, 'b) interface

  val compare: t -> t -> int
end

module MakeOperation(T: Irmin.Contents.S): OPERATION with module Val = T = struct
  module Val = T

  type t = | B: (Val.t, 'a) NamedOp.t -> t
  type 'a matched_implementation = (Val.t, 'a) NamedOp.t * 'a
  type boxed_mi = | E: 'a matched_implementation -> boxed_mi

  let return = BaseType
  let (@->) p f = ParamType (p, f)

  (* let apply: ('v,'a) interface -> 'a -> (('v,'a) interface * ('v, 'a) params) = fun interface ->
   *   match interface with
   *   | Unary funtype -> let rec construct_params p = match p with
   *       | 
   *     in construct_params
   *   | _ -> invalid_arg "Cannot apply complex interfaces" *)

  let declare name typ: ('v,'a) interface =
    Unary {NamedOp.name = name; NamedOp.typ = typ}

  let compare a b =
    match (a, b) with
    | B {NamedOp.name = n1; _}, B {NamedOp.name = n2; _} ->
      String.compare n1 n2
end


exception Invalid_description of string
module Description(Val: Irmin.Contents.S) = struct
  module Op = MakeOperation(Val)
  module OpSet = Set.Make(Op)

  (* A description is a set of operations *)
  type 'i t = OpSet.t

  let describe unboxed = Op.B unboxed

  let rec interface_to_list: type i. (Val.t, i) interface -> Op.t list = fun interface ->
    match interface with
    | Unary t -> [Op.B t]
    | Complex (t, ts) -> (Op.B t)::interface_to_list(ts)

  let (@): type a b. (Val.t, a) interface -> (Val.t,b) interface -> (Val.t, a * b) interface = fun i is ->
      match i with
      | Unary t -> Complex (t, is)
      | _ -> invalid_arg "Cannot compose implementations like this"

  let define: (Val.t, 'i) interface -> 'i t = fun interface ->
    let l = interface_to_list interface in
    let len = List.length l in
    let set = OpSet.of_list l in

    if (OpSet.cardinal set != len) then
      raise @@ Invalid_description "Duplicate function name contained in list"
    else set

  let valid_name name d =
    OpSet.exists (fun b -> match b with
        | Op.B unboxed -> (NamedOp.name unboxed) == name) d
end

module type IMPL_MAKER = sig
  module S: Irmin.Contents.S
  module Op: OPERATION with module Val = S

  type 'i t
  (** The type of implementations with type structure 'i from type 'a to 'a *)

  val (@): (Op.Val.t,'a) implementation
    -> (Op.Val.t,'b) implementation
    -> (Op.Val.t,'a * 'b) implementation

  val define: (Op.Val.t,'i) implementation -> 'i t
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
  let (@) (proto_interface, func) (acc_interface, acc_functions) =

      match proto_interface with
    | Unary proto -> (Complex (proto, acc_interface), (func, acc_functions))
    | _ -> invalid_arg "Cannot compose implementations like this"

  (* Helper function to add a type declaration and function to a hashtable *)
  let add_to_hashtable h typ func =
    let n = NamedOp.name typ in (* the name of the function *)
    let boxed = Op.E (typ, func) in (* the format we store in the hashmap *)

    (match Hashtbl.find_opt h n with

     | Some _ -> raise @@ Invalid_description
         ("Duplicate function name (" ^ n ^ ") in implementation")

     (* This name has not been used before *)
     | None -> Hashtbl.add h n boxed)

  (* Simply convert the list to a hashtable, return an exception if there
     are any duplicate function names *)
  let define fns =

    let h = Hashtbl.create 10 in

    let rec aux: type a. (Op.Val.t, a) implementation -> unit = fun impl -> match impl with
      | (Unary t, f) -> add_to_hashtable h t f
      | (Complex (t, ts), (f, fs)) -> add_to_hashtable h t f; aux (ts, fs)

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




