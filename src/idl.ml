module Interface = struct
  type description = {
    name: string;
    description: string;
    version: Rpc.Version.t
  }
end

module Param = struct
  type 'a t = {
    name: string;
    typedef: 'a Irmin.Type.t;
  }

  type boxed = Boxed: 'a t -> boxed

  let mk ~name typedef = {name; typedef}
end

module type RPC = sig
  type implementation
  type 'a res
  type ('a, 'b) comp
  type _ fn

  val ( --> ) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val implement : Interface.description -> implementation
  val returning : 'a Param.t -> ('a, 'b) comp fn
  val declare : name:string -> description:string -> 'a fn -> 'a res
end

module type MONAD = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ret : 'a -> 'a t
end

let get_argument call name =
  let ps = call.Rpc.params in
    match List.partition (fun (x, _) -> x = name) ps with
    | (_, arg) :: dups, others ->
        Result.Ok (arg, {call with Rpc.params=(dups @ others)})
    | _, _ ->
        Result.Error (`Msg (Printf.sprintf "Expecting named argument '%s'" name))

module Make (M : MONAD) = struct
  module type RPCTRANSFORMER = sig
    type 'a box
    type ('a, 'b) resultb = ('a, 'b) Result.result box
    type rpc_func = Rpc.call -> Rpc.response

    val lift : ('a -> 'b M.t) -> 'a -> 'b box
    val bind : 'a box -> ('a -> 'b M.t) -> 'b box
    val ret : 'a -> 'a box
    val get : 'a box -> 'a M.t
    val ( !@ ) : 'a box -> 'a M.t
    val put : 'a M.t -> 'a box
    val ( ~@ ) : 'a M.t -> 'a box
  end

  module T = struct
    type 'a box = {box: 'a M.t}
    type ('a, 'b) resultb = ('a, 'b) Result.result box
    type rpc_func = Rpc.call -> Rpc.response

    let lift f x = {box = f x}
    let bind {box = x} f = {box = M.bind x f}
    let ret x = {box = M.ret x}
    let get {box = x} = x
    let ( !@ ) = get
    let put x = {box= x}
    let ( ~@ ) = put
  end

  module ErrM  = struct
    let ret x = T.put (M.ret (Result.Ok x))
    let ret_err e = T.put (M.ret (Result.Error e))
    let checked_bind x f f1 =
      T.bind x
        T.(function Result.Ok x -> !@(f x) | Result.Error x -> !@(f1 x))

    let bind x f = checked_bind x f ret_err
    let ( >>= ) x f = bind x f
  end

  type client_implementation = unit
  type server_implementation = (string, T.rpc_func option) Hashtbl.t

  module Client () = struct
    type implementation = client_implementation
    type 'a res = T.rpc_func -> 'a
    type ('a, 'b) comp = ('a, 'b) T.resultb

    type _ fn =
      | Function: 'a Param.t * 'b fn -> ('a -> 'b) fn
      | Returning: 'a Param.t -> ('a, 'b) comp fn

    let description = ref None

    let implement x = description := Some x; ()
    let returning a = Returning a

    let ( --> ) t f = Function (t, f)
    let declare ~name:_ ~description:_ _ (_: T.rpc_func) = invalid_arg "TODO"
  end

  module Server () = struct
    type implementation = server_implementation
    type ('a, 'b) comp = ('a, 'b) T.resultb
    type 'a res = 'a -> unit

    type _ fn =
      | Function: 'a Param.t * 'b fn -> ('a -> 'b) fn
      | Returning: 'a Param.t -> ('a, 'b) comp fn
    [@@deriving sexp]

    let funcs = Hashtbl.create 5
    let description = ref None

    let implement x : implementation = description := Some x; funcs
    let returning a  = Returning a

    let ( --> ) t f = Function (t, f)

    exception NoDescription
    exception MarshalError of string

    let declare : name:string -> description:string -> 'a fn -> 'a res =

      fun ~name ~description:_ ty ->

      (* We do not know the wire name yet as the description may still be unset *)
      Hashtbl.add funcs name None;

      fun impl ->
        if !description = None then raise NoDescription;
        let rpcfn =
          let rec inner : type a. a fn -> a -> Rpc.call -> Rpc.response = fun f impl call ->
            try
              match f with
              | Function (t, f) ->
                let (arg_rpc, call') =
                  match get_argument call t.Param.name with
                  | Result.Ok arg -> arg
                  | Result.Error (`Msg m) -> raise (MarshalError m)
                in
                (* let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in *)
                let z = Result.Ok arg_rpc in
                let arg =
                  match z with
                  | Result.Ok arg -> arg
                  | Result.Error (`Msg m) -> raise (MarshalError m)
                in
                inner f (impl arg) call'
              | Returning _ (*t*) -> Rpc.success "marshalled param"
            with e -> raise e
          in inner ty impl
        in

      (* The wire name might be different from the name *)
      Hashtbl.remove funcs name;
      Hashtbl.add funcs name (Some rpcfn)
  end
end

module IdM = struct
  type 'a t = T of 'a

  let lift f x = T (f x)
  let bind (T x) f = f x
  let ret x = T x
  let ( >>= ) = bind
  let run (T x) = x
end
