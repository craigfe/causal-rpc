module type S = sig
  module Store: Store.S
  module Value = Store.Value

  type t
  (** A client *)

  val empty: ?directory:string -> remote_uri:string -> name:string -> t Lwt.t

  val rpc: ?timeout:float
    -> (Value.t,'a,'p) Interface.NamedOp.t
    -> (Value.t, 'a) Interface.params
    -> t -> t Lwt.t
end

module Make (Store: Store.S): S
  with module Store = Store
