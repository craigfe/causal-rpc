module type S = sig
  module Store: Store.S
  module Value = Store.Value

  type t
  (** A client *)

  val empty: ?directory:string
    -> remote_uri:string
    -> local_uri:string
    -> name:string
    -> initial:Value.t
    -> t Lwt.t

  val rpc: ?timeout:float
    -> Value.t Operation.rpc
    -> t -> Value.t Lwt.t

  val output: t -> unit Lwt.t
end

module Make (Store: Store.S): S
  with module Store = Store
