
(** A task is an operation, a list of parameters and a key specifying the value
    on which to perform the operation *)

module type S = sig
  type key = string
  (** The type of the map keys *)

  type t
  (** The type of maps from type [key] to type [value] *)

  module Description: Interface.DESC
  module Value = Description.Val

  (* The contents of the Irmin store are determined by the store module*)
  module Contents: Irmin.Contents.S with type t = Value.t Store.contents
  module B: Backend.S
  module IrminStore: Irmin_git.S
    with type key = string list
     and type step = string
     and type contents = Contents.t
     and type branch = string

  module Sync: Irmin.SYNC with type db = IrminStore.t
  module JobQueue: Store.JOB_QUEUE with module Store = IrminStore
  module Operation: Interface.OPERATION with module Val = Value

  type 'a params = (Value.t, 'a) Interface.params

  exception Internal_type_error
  exception Store_error of IrminStore.write_error

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

  val map: ?timeout:float -> (Value.t,'a,'p) Interface.NamedOp.t -> 'a params -> t -> t Lwt.t
  (** [map m] returns a map with the same domain as [m] in which
      the associated value [a] of all bindings of [m] have been
      replaced by the result of applying _a_ function to [a] *)
end

module Make
    (BackendMaker: Backend.MAKER)
    (GitBackend: Irmin_git.G)
    (Desc: Interface.DESC)
    (JQueueMake: functor
       (Val: Irmin.Contents.S)
       (B: Backend.S
        with type Store.key = Irmin.Path.String_list.t
         and type Store.step = string
         and module Store.Key = Irmin.Path.String_list
         and type Store.contents = Val.t Store.contents
         and type Store.branch = string)
       -> (Store.JOB_QUEUE with module Store = B.Store)): S
  with module Description = Desc
   and module Operation = Interface.MakeOperation(Desc.Val)
(** Functor building an implementation of the map structure given:
     - a value for the map to contain
     - a set of operations on that type
     - a queue type
     - a job queue implementation *)
