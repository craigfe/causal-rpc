(** Module containing the various configuration parameters of a worker *)
module Config : sig
  type t

  val make: ?log_source:bool
    -> ?random_selection:bool
    -> ?batch_size:int
    -> ?thread_count:int
    -> ?name:string
    -> ?poll_freq:float
    -> ?two_phase:bool
    -> ?one_shot:bool
    -> unit -> t
  (** Construct a worker Config object with specified parameters *)

  val log_source: t -> bool
  (** [log_source] is true if the worker should print its name in log messages *)

  val random_selection: t -> bool
  (** [random_selection] is true if the worker selects randomly from the
      task set (this is the default behaviour). Otherwise, the worker selections
      are deterministic. *)

  val batch_size: t -> int
    (** [batch_size] is the number of items to consume from the task set in a
        single RTT with the server. *)

  val thread_count: t -> int
  (** [thread_count] is the number of worker threads to be kept active by the
      worker. Currently unsupported due to the lack of support for multi-core OCaml *)

  val name: t -> string
  (** [name] is the author name of all commits made by the worker, as well as the
      printed log source *)

  val poll_freq: t -> float
  (** [poll_freq] is the number of seconds to wait between polling the server for
      new jobs *)

  val two_phase: t -> bool
  (** [two_phase] is true if the intermediate consumption phase is disabled *)

  val one_shot: t -> bool
  (** A [one_shot] worker gives up immediately after finding an empty job
      queue at the server. Used for demonstration purposes. *)
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
