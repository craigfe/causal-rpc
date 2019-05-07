
(** A task is an operation, a list of parameters and a key specifying the value
    on which to perform the operation *)

module type S = sig
  type key = string
  (** The type of the map keys *)

  type t
  (** The type of maps from type [key] to type [value] *)

  module Store: Store.S
  module Sync = Store.IrminSync
  module Value = Store.Value

  type 'a params = (Value.t, 'a) Operation.params

  val of_store: Sync.db -> t
  (** Return the map corresponding to an underlying store representation *)

  val to_store: t -> Sync.db
  (** Return the underlying store representation. TODO: remove. Nothing should need access
      to this, but it is currently being used by the worker. *)

  val empty: ?directory:string -> ?remote_uri:string -> unit -> t Lwt.t
  (** The empty map. If no directory is passed, then one will be generated randomly.
      [remote_uri] is the location of the remote to use. If none is supplied, the
      operations will not be pushed to a remote. *)

  val is_empty: t -> bool Lwt.t
  (** Test whether a map is empty or not. *)

  val mem: key -> t -> bool Lwt.t
  (** [mem x m] returns true iff [m] contains a binding for [x] *)

  val add: ?message:string -> key -> Value.t -> t -> t Lwt.t
  (** [add x y m] returns a map containing the same bindings as [m],
      plus a binding of [x] to [y]. If [x] was already bound in [m],
      its previous binding is replaced. *)

  val add_all: ?message:string -> (key * Value.t) list -> t -> t Lwt.t
  (** [add bind_list m] returns a map containing the same bindings as [m],
      plus bindings from k to v for all (k, v) in [bind_list]. If any k
      was already bound in [m], its previous binding is replaced. *)

  val find: key -> t -> Value.t Lwt.t
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val remove: key -> t -> t Lwt.t
  (** [remove x m] returns a map containing the same bindings as [m],
      except for [x] which is unbound in the returned map. *)

  val size: t -> int Lwt.t
  (** Return the number of bindings in the map *)

  val keys: t -> key list Lwt.t
  (** Return a list of keys in the map *)

  val values: t -> Value.t list Lwt.t
  (** Return a list of values in the map *)

  val map: ?timeout:float -> ?polling:bool -> Value.t Remote.t -> t -> t Lwt.t
  (** [map m] returns a map with the same domain as [m] in which
      the associated value [a] of all bindings of [m] have been
      replaced by the result of applying _a_ function to [a] *)

  val start: t -> unit Lwt.t
  (** Begin running a daemon thread to process RPC jobs in the job queue *)
end

module Make (Store: Store.S)
    (Impl: Interface.IMPL with module Val = Store.Value)
    : S with module Store = Store
