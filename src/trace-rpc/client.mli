module type S = sig
  module Store: Store.S
  module Value = Store.Value

  type t
  (** The type of a CausalRPC client *)

  val clear_caches: t -> unit Lwt.t
  (** When using the filesystem backend, clear the in-memory caches *)

  val empty: ?directory:string
    -> remote_uri:string
    -> local_uri:string
    -> name:string
    -> initial:Value.t
    -> t Lwt.t
  (** [empty ~remote_uri ~local_uri ~name ~initial] returns a Client store
      initialised to the value of [initial], pointing at a server at
      [remote_uri] with a reverse pointer [local_uri]. The string [name] is
      used to author all commits in the repository.contents *)

  val rpc: ?timeout:float
    -> Value.t Remote.t
    -> t -> Value.t Lwt.t
  (** Issue a given RPC to the remote, returning the value contained within
      the store once the RPC is complete. *)

  val output: t -> unit Lwt.t
  (* When using the in-memory store, dump the contents of the store to the
     [directory] supplied during initialisation (for debug), otherwise fail. *)
end

module Make (Store: Store.S): S
  with module Store = Store
