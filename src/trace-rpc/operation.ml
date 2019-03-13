type (_,_) params =
  | Unit : ('v, 'v -> 'v) params
  | Param : ('p Type.t * 'p * ('v,'a) params) -> ('v, 'p -> 'a) params

type (_,_,_) prototype =
  | BaseType : ('a, 'a -> 'a, ('a, 'a -> 'a) params) prototype
  | ParamType : ('t Type.t * ('a, 'b, ('a, 'p) params) prototype)
      -> ('a, ('t -> 'b), ('a, 't -> 'p) params) prototype

module NamedOp = struct
  type ('v, 'a, 'p) t = {
    name: string;
    typ: ('v, 'a, 'p) prototype;
  }

  let name {name = n; _} = n
  let typ {name = _; typ = t} = t
end

type (_,_) interface =
  | Unary : ('v,'a,'p) NamedOp.t -> ('v,'a) interface
  | Complex : (('v,'a,'p) NamedOp.t * ('v, 'b) interface) -> ('v, 'a * 'b) interface

type ('v,'a) implementation = ('v,'a) interface * 'a

module type OPERATION = sig
  module Val: Irmin.Contents.S

  type t = | B: (Val.t, 'a, 'p) NamedOp.t -> t
  type ('a, 'p) matched_implementation = (Val.t, 'a, 'p) NamedOp.t * 'a
  type boxed_mi = | E: ('a, 'p) matched_implementation -> boxed_mi

  val flatten_params: (Val.t, 'a) params -> Type.Boxed.t list
  val pass_params: ?src:Logs.src -> boxed_mi -> Type.Boxed.box list -> Val.t -> Val.t

  (* apply multiply_op 5 10 *)
  (* val map: (Value.t,'a) Interface.interface -> 'a params -> t -> t Lwt.t *)

  (* ('v, 'a) interface -> 'a*)
  (* val apply: ('v,'a,'p) interface -> 'a -> (('v,'a,'p) interface * ('v, 'a) params) *)
  val return: ('a, 'a -> 'a, ('a, 'a -> 'a) params) prototype

  val (@->): 't Type.t
    -> ('a, 'b, ('a, 'p) params) prototype
    -> ('a, ('t -> 'b), ('a, 't -> 'p) params) prototype

  val declare: string -> (Val.t, 'b, 'p) prototype -> (Val.t, 'b, 'p) NamedOp.t

  val compare: t -> t -> int
end

module MakeOperation(T: Irmin.Contents.S): OPERATION with module Val = T = struct
  module Val = T

  type t = | B: (Val.t, 'a, 'p) NamedOp.t -> t
  type ('a, 'p) matched_implementation = (Val.t, 'a, 'p) NamedOp.t * 'a
  type boxed_mi = | E: ('a, 'p) matched_implementation -> boxed_mi

  let rec flatten_params: type v a. (v, a) params -> Type.Boxed.t list = fun ps ->
    match ps with
    | Unit -> []
    | Param (typ, p, ps) -> ((Type.Boxed.box typ p)::flatten_params(ps))

  (* We have a function of type (param -> ... -> param -> val -> val).
     Here we take the parameters that were passed as part of the RPC and recursively apply them
     to the function implementation until we are left with a function of type (val -> val). *)
  let pass_params ?src boxed_mi params =
    match boxed_mi with
    | E matched_impl ->
      let (unboxed, func) = matched_impl in
      let func_type = NamedOp.typ unboxed in

      (* We take a function type and a function _of that type_, and recursively apply parameters
         to the function until it reaches 'BaseType', i.e. val -> val *)
      let rec aux: type a p.
        (Val.t, a, p) prototype
        -> a
        -> Type.Boxed.t list
        -> (Val.t -> Val.t) = fun func_type func params ->

        match func_type with
        | BaseType -> (Logs.debug ?src (fun m -> m "Reached base type"); match params with
          | [] -> (fun x ->
              Logs.debug ?src (fun m -> m "Executing val -> val level function");
              let v = func x in
              Logs.debug ?src (fun m -> m "Function execution complete");
              v)
          | _ -> raise @@ Exceptions.Malformed_params "Too many parameters")

        | ParamType (typ, nested_type) -> (Logs.debug ?src (fun m -> m "Nested type"); match params with
          | (x::xs) -> aux nested_type (func (Type.Boxed.unbox typ x)) xs
          | [] -> raise @@ Exceptions.Malformed_params "Not enough parameters")

      in aux func_type func params

  let return = BaseType
  let (@->) p f = ParamType (p, f)

  (* let apply: ('v,'a) interface -> 'a -> (('v,'a) interface * ('v, 'a) params) = fun interface ->
   *   match interface with
   *   | Unary funtype -> let rec construct_params p = match p with
   *       | 
   *     in construct_params
   *   | _ -> invalid_arg "Cannot apply complex interfaces" *)

  let declare name typ: (Val.t, 'a, 'p) NamedOp.t =
    {NamedOp.name = name; NamedOp.typ = typ}

  let compare a b =
    match (a, b) with
    | B {NamedOp.name = n1; _}, B {NamedOp.name = n2; _} ->
      String.compare n1 n2
end
