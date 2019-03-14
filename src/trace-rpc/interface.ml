include Operation
open Exceptions

module type IMPL_MAKER = sig
  module S: Irmin.Contents.S
  module Op: Operation.S with module Val = S

  type 'i t
  (** The type of implementations with type structure 'i from type 'a to 'a *)

  val (@): ((Op.Val.t, 'a, 'p) NamedOp.t * 'a)
    -> (Op.Val.t, 'b) implementation
    -> (Op.Val.t, 'a * 'b) implementation

  val finally: ((Op.Val.t, 'a, 'p) NamedOp.t * 'a) -> (Op.Val.t, 'a) implementation

  val define: (Op.Val.t,'i) implementation -> 'i t
  (** Construct an RPC implementation from a list of pairs of operations and
      implementations of those operations *)

  val find_operation_opt: string -> 'i t -> Op.boxed_mi option
  (** Retreive an operation from an implementation *)
end

module MakeImplementation(T: Irmin.Contents.S): IMPL_MAKER
  with module S = T
   and module Op = Operation.Make(T) = struct
  module S = T
  module Op = Operation.Make(T)

  (* An implementation is a map from operations to type-preserving functions
     with string parameters *)
  type 'i t = (string, Op.boxed_mi) Hashtbl.t

  let finally: type a p. ((Op.Val.t, a, p) NamedOp.t * a) -> (Op.Val.t, a) implementation =
    fun (prototype, operation) -> (Unary prototype, operation)

  (* Combine two implementations by aggregating the prototypes and storing the
     functions as nested pairs. We require that the first implementation contains only a
     single operation. *)
  let (@) (prototype, operation) (acc_interface, acc_functions) =
    Complex (prototype, acc_interface), (operation, acc_functions)

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

module type IMPL = sig
  module Val: Irmin.Contents.S
  type shape
  val api: shape MakeImplementation(Val).t
end

