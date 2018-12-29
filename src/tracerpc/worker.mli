module Config : sig
  type t

  val make: ?log_source:bool
    -> ?random_selection:bool
    -> ?batch_size:int
    -> ?thread_count:int
    -> ?name:string
    -> ?poll_freq:float
    -> ?two_phase:bool
    -> unit -> t

  val log_source: t -> bool
  val random_selection: t -> bool
  val batch_size: t -> int
  val thread_count: t -> int
  val name: t -> string
  val poll_freq: t -> float
  val two_phase: t -> bool
end

module type W = sig
  val run:
    ?switch:Lwt_switch.t ->
    ?config:Config.t ->
    ?dir:string ->
    client:string -> unit -> unit Lwt.t
    (** run c () returns a promise of a worker computation with a commit author
        ~name that listens for work requests from client c and performs the work
        in repository ~dir *)
end

module Make
    (Map: Map.S)
    (Impl: Interface.IMPL with module Val = Map.Value): W
(** Functor building a worker that operates on the map structure
    given an equality type and a set of operations on that type. *)
