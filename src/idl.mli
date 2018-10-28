(* The Idl module allows defining RPC interfaces *)
module Interface : sig
  type description = {
    name: string;
    description: string list;
    version: Rpc.Version.t;
  }
end

(* Each RPC takes some number of 'a Param.t values as arguments *)
module Param : sig
  type 'a t = {
    name: string;
    typedef: 'a Irmin.Type.t
  }

  (* Boxed type to allow listing *)
  type boxed = Boxed: 'a t -> boxed

  (* Create a Param.t out of a Irmin serialisable type *)
  val mk: name:string -> 'a Irmin.Type.t -> 'a t
end

(* Specialisation modules must conform to this type *)
module type RPC = sig
  type implementation
  type 'a res
  type ('a, 'b) comp

  (* GADT for RPC type*)
  type _ fn

  val ( --> ) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val implement : Interface.description -> implementation
  val returning : 'a Param.t -> ('a, 'b) comp fn
  val declare: string -> string list -> 'a fn -> 'a res
end

(* Simple monad definition *)
module type MONAD = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ret : 'a -> 'a t
end

(* Module for generating clients and servers *)
module Make(M: MONAD) : sig
  type client_implementation
  type server_implementation

  (* Monadic transformation type for RPC interfaces*)
  module type RPCTRANSFORMER = sig
    type 'a box
    type ('a, 'b) resultb = ('a, 'b) Result.result box
    type rpc_func = Rpc.call -> Rpc.response M.t

    val lift : ('a -> 'b M.t) -> 'a -> 'b box
    val bind : 'a box -> ('a -> 'b M.t) -> 'b box
    val ret : 'a -> 'a box
    val get : 'a box -> 'a M.t
    val ( !@ ) : 'a box -> 'a M.t
    val put : 'a M.t -> 'a box
    val ( ~@ ) : 'a M.t -> 'a box
  end

  module T : RPCTRANSFORMER

  module Client () : sig
    include RPC
      with type implementation = client_implementation
       and type 'a res = T.rpc_func -> 'a
       and type ('a, 'b) comp = ('a, 'b) T.resultb
  end

  module Server () : sig
    include RPC
      with type implementation = server_implementation
       and type 'a res = 'a -> unit
       and type ('a, 'b) comp = ('a, 'b) T.resultb
  end
end
